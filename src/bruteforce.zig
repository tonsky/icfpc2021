const std = @import("std");

const Point = struct { x: i64, y: i64 };
const Edge = struct { from: usize, to: usize };

const Bonus = struct {
    bonus: []const u8,
    problem: usize, 
    position: Point
};

const Problem = struct {
    id: u64,
    bonuses: []Bonus,
    hole: []Point,
    epsilon: u64,
    edges: []Edge,
    vertices: []Point,
    minx: i64,
    maxx: i64,
    miny: i64,
    maxy: i64
};

const Orientation = enum { colinear, counterclockwise, clockwise };

pub fn load_problem(id: u64, allocator: *std.mem.Allocator) !Problem {
    const path = try std.fmt.allocPrint(allocator, "problems/{}.problem", .{ id });
    defer allocator.free(path);
    std.debug.print("Loading {s}\n", .{ path });
    var file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const stat = try file.stat();
    var contents = try allocator.alloc(u8, stat.size);
    defer allocator.free(contents);
    var bytes_read = try file.readAll(contents);

    var parser = std.json.Parser.init(allocator, false);
    defer parser.deinit();
    var tree = try parser.parse(contents);
    defer tree.deinit();

    var hole = try allocator.alloc(Point, tree.root.Object.get("hole").?.Array.items.len);
    for (tree.root.Object.get("hole").?.Array.items) |vertex, i| {
        hole[i] = Point{ .x = vertex.Array.items[0].Integer, .y = vertex.Array.items[1].Integer };
    }

    var edges = try allocator.alloc(Edge, tree.root.Object.get("figure").?.Object.get("edges").?.Array.items.len);
    for (tree.root.Object.get("figure").?.Object.get("edges").?.Array.items) |vertex, i| {
        var v1 = @intCast(u64, vertex.Array.items[0].Integer);
        var v2 = @intCast(u64, vertex.Array.items[1].Integer);
        // edges[i] = Edge{ .from = std.math.min(v1, v2), .to = std.math.max(v1, v2) };
        edges[i] = Edge{ .from = v1, .to = v2 };
    }

    var vertices = try allocator.alloc(Point, tree.root.Object.get("figure").?.Object.get("vertices").?.Array.items.len);
    for (tree.root.Object.get("figure").?.Object.get("vertices").?.Array.items) |vertex, i| {
        vertices[i] = Point{ .x = vertex.Array.items[0].Integer, .y = vertex.Array.items[1].Integer };
    }

    var bonuses = try allocator.alloc(Bonus, tree.root.Object.get("bonuses").?.Array.items.len);
    for (tree.root.Object.get("bonuses").?.Array.items) |bonus, i| {
        bonuses[i] = Bonus{ 
            .bonus = try allocator.dupe(u8, bonus.Object.get("bonus").?.String),
            .problem = @intCast(u64, bonus.Object.get("problem").?.Integer),
            .position = Point{
                .x = bonus.Object.get("position").?.Array.items[0].Integer,
                .y = bonus.Object.get("position").?.Array.items[1].Integer
            }
        };
    }

    var minx: i64 = 0x7FFFFFFFFFFFFFFF;
    var maxx: i64 = 0;
    var miny: i64 = 0x7FFFFFFFFFFFFFFF;
    var maxy: i64 = 0;
    for (hole) |v| {
        if (v.x > maxx) maxx = v.x;
        if (v.y > maxy) maxy = v.y;
        if (v.x < minx) minx = v.x;
        if (v.y < miny) miny = v.y;
    }

    return Problem{
        .id = id,
        .hole = hole,
        .bonuses = bonuses,
        .epsilon = @intCast(u64, tree.root.Object.get("epsilon").?.Integer),
        .vertices = vertices,
        .edges = edges,
        .minx = minx,
        .miny = miny,
        .maxx = maxx + 1,
        .maxy = maxy + 1
    };
}

fn orientation(p: Point, q: Point, r: Point) Orientation {
    const res = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y);
    if (res == 0) {
        return Orientation.colinear;
    } else if (res < 0) {
        return Orientation.counterclockwise;
    } else {
        return Orientation.clockwise;
    }
}

// if q lies on line segment 'pr'
fn onSegment(p: Point, q: Point, r: Point) bool {
    return orientation(p, q, r) == Orientation.colinear and onSegmentImpl(p, q, r);
}

// if q lies on line segment 'pr'
fn onSegmentImpl(p: Point, q: Point, r: Point) bool {
    return q.x <= std.math.max(p.x, r.x)
       and q.x >= std.math.min(p.x, r.x)
       and q.y <= std.math.max(p.y, r.y)
       and q.y >= std.math.min(p.y, r.y);
}

// adapted from https://www.geeksforgeeks.org/check-if-two-given-line-segments-intersect/
fn intersect(p1: Point, q1: Point, p2: Point, q2: Point) bool {
    var d1 = orientation(p1, q1, p2);
    var d2 = orientation(p1, q1, q2);
    var d3 = orientation(p2, q2, p1);
    var d4 = orientation(p2, q2, q1);
    // std.debug.print("d1 {} d2 {} d3 {} d4 {}\n", .{d1, d2, d3, d4});
    // General case
    if (d1 != d2 and d3 != d4)
        return true;
  
    // p1, q1 and p2 are colinear and p2 lies on segment p1q1
    if (d1 == Orientation.colinear and onSegmentImpl(p1, p2, q1))
        return true;
  
    // p1, q1 and q2 are colinear and q2 lies on segment p1q1
    if (d2 == Orientation.colinear and onSegmentImpl(p1, q2, q1))
        return true;
  
    // p2, q2 and p1 are colinear and p1 lies on segment p2q2
    if (d3 == Orientation.colinear and onSegmentImpl(p2, p1, q2))
        return true;
  
    // p2, q2 and q1 are colinear and q1 lies on segment p2q2
    if (d4 == Orientation.colinear and onSegmentImpl(p2, q1, q2))
        return true;
  
    return false;
}

pub fn is_parallel(p1: Point, q1: Point, p2: Point, q2: Point) bool {
    return (p1.x - q1.x) * (p2.y - q2.y) == (p1.y - q1.y) * (p2.x - q2.x);
    // return std.math.atan2(i64, p1.x - q1.x, p1.y - q1.y) == std.math.atan2(i64, p2.x - q2.x, p2.y - q2.y);
}

pub fn intersect_really(p1: Point, q1: Point, p2: Point, q2: Point) bool {
    if (std.meta.eql(p1, p2) or std.meta.eql(p1, q1) or std.meta.eql(q1, p2) or std.meta.eql(q1, q2))
        return false;
    if (is_parallel(p1, q1, p2, q2))
        return false;
    if (onSegment(p2, p1, q2) or onSegment(p2, q1, q2))
        return false;
    return intersect(p1, q1, p2, q2);
}

pub fn distance(p: Point, q: Point) f64 {
    return @intToFloat(f64, (p.x - q.x) * (p.x - q.x) + (p.y - q.y) * (p.y - q.y));
}

pub fn calc_edges_matrix(problem: Problem, allocator: *std.mem.Allocator) ![]f64 {
    const res: []f64 = try allocator.alloc(f64, problem.vertices.len * problem.vertices.len);
    std.mem.set(f64, res, 0);
    for (problem.edges) |edge| {
        const d = distance(problem.vertices[edge.from], problem.vertices[edge.to]);
        res[problem.vertices.len * edge.from + edge.to] = d;
        res[problem.vertices.len * edge.to + edge.from] = d;
    }
    return res;
}

pub fn calc_inside(problem: Problem, allocator: *std.mem.Allocator) ![]bool {
    const inside: []bool = try allocator.alloc(bool, @intCast(usize, (problem.maxx - problem.minx) * (problem.maxy - problem.miny)));
    std.mem.set(bool, inside, false);

    var y: i64 = problem.miny;
    while (y < problem.maxy) : (y += 1) {
        var x: i64 = problem.minx;
        while (x < problem.maxx) : (x += 1) {
            const inside_idx = @intCast(usize, (y - problem.miny) * (problem.maxx - problem.minx) + (x - problem.minx));
            const point = Point{ .x = x, .y = y };
            const outside = Point{ .x = 530711, .y = 378353 }; // two different large primes, thx bigprimes.org
            var intersections: u64 = 0;
            var from: usize = 0;
            while (from < problem.hole.len) : (from += 1) {
                const to: usize = try std.math.mod(usize, from + 1, problem.hole.len);
                const p0 = problem.hole[from];
                const p1 = problem.hole[to];
                if (onSegment(p0, point, p1)) {
                    inside[inside_idx] = true;
                    break;
                } else if (intersect(point, outside, p0, p1) and !onSegment(point, p1, outside)) {
                    intersections += 1;
                }
            }
            if (!inside[inside_idx])
                inside[inside_idx] = 1 == try std.math.mod(u64, intersections, 2);
            // if (inside[inside_idx])
            //     std.debug.print("▓", .{})
            // else
            //     std.debug.print("░", .{});
        }
        // std.debug.print("\n", .{});
    }
    return inside;
}

pub fn calc_score(problem: Problem, vertices: []Point) i64 {
    var from: usize = 0;
    var res: i64 = 0;
    while (from < problem.hole.len) : (from += 1) {
        var min_d: f64 = -1;
        for (vertices) |vertex| {
            const d = distance(problem.hole[from], vertex);
            if (min_d == -1 or d < min_d) {
                min_d = d;
            }
        }
        res += @floatToInt(i64, min_d);
    }
    return res;
}

pub fn save_solution(problem: Problem, vertices: []Point, allocator: *std.mem.Allocator) !void {
    const score = calc_score(problem, vertices);
    const dir = try std.fmt.allocPrint(allocator, "solutions/{}", .{ problem.id });
    defer allocator.free(dir);
    try std.fs.cwd().makePath(dir);
    const path = try std.fmt.allocPrint(allocator, "{s}/{}.solution", .{ dir, score });
    defer allocator.free(path);
    const file = std.fs.cwd().createFile(path, .{ .exclusive = true }) catch |err| {
        std.debug.print("File exists: {s}/{s}\n", .{ dir, path });
        return;
    };
    std.debug.print("Saving {s}\n", .{ path });
    defer file.close();
    const writer = file.writer();
    try writer.writeAll("{\"vertices\":[");
    for (vertices) |vertex, i| {
        try writer.print("[{},{}]", .{ vertex.x, vertex.y });
        if (i < vertices.len - 1)
            try writer.writeAll(",");
    }
    try writer.writeAll("],\"edges\":[");
    for (problem.edges) |edge, i| {
        try writer.print("[{},{}]", .{ edge.from, edge.to });
        if (i < problem.edges.len - 1)
            try writer.writeAll(",");
    }
    try writer.writeAll("]}");
}

pub fn main() !void {
    const timer = std.time.Timer.start() catch unreachable;

    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;
    // var allocator = std.testing.allocator;

    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    var id = try std.fmt.parseInt(u64, args[1], 10);
    var problem = try load_problem(id, allocator);
    defer std.json.parseFree(Problem, problem, .{ .allocator = allocator });

    std.debug.print("Hole {}..{} x {}..{}, edges {}, vertices {}\n", .{problem.minx, problem.maxx, problem.miny, problem.maxy, problem.edges.len, problem.vertices.len});
    const inside = try calc_inside(problem, allocator);
    defer allocator.free(inside);

    const edges_matrix = try calc_edges_matrix(problem, allocator);
    defer allocator.free(edges_matrix);

    const vertices = problem.vertices.len;
    const stack = try allocator.alloc(Point, vertices);
    defer allocator.free(stack);

    // const order = try allocator.alloc(usize, vertices);
    // defer allocator.free(order);
    // order = 1, 3, 5

    var stack_idx: usize = 0;
    var epsilon = @intToFloat(f64, problem.epsilon) / 1000000.0;
    stack[0] = Point{ .x = problem.minx, .y = problem.miny };
    var best_score: i64 = -1;
    var best_stack: []Point = try allocator.alloc(Point, vertices);
    defer allocator.free(best_stack);
    outer: while (true) {
        const x = stack[stack_idx].x;
        const y = stack[stack_idx].y;

        // std.debug.print("x={} y={} stack_idx={} ", .{ x, y, stack_idx });
        // for (stack) |p| {
        //     std.debug.print("[{},{}], ", .{ p.x, p.y });
        // }
        // std.debug.print("\n", .{});

        if (y >= problem.maxy) {
            if (stack_idx == 0) {
                break;
            } else {
                stack_idx -= 1;
                stack[stack_idx].x += 1;
                continue;
            }
        }

        if (x >= problem.maxx) {
            stack[stack_idx].x = problem.minx;
            stack[stack_idx].y += 1;
            continue;
        }

        const inside_idx = @intCast(usize, (y - problem.miny) * (problem.maxx - problem.minx) + (x - problem.minx));
        if (!inside[inside_idx]) {
            stack[stack_idx].x += 1;
            continue;
        }

        // if edges have acceptable length
        var i: usize = 0;
        while (i < stack_idx) : (i += 1) { // all vertices already set
            const old_len = edges_matrix[i * vertices + stack_idx];
            if (old_len > 0) { // edge exist
                const new_len = distance(stack[i], stack[stack_idx]);
                if (std.math.absFloat(new_len / old_len - 1.0) > epsilon) {
                    if (new_len > old_len) { // optimization!
                        const nexty = stack[i].y - @floatToInt(i64, std.math.ceil(old_len * (1 + epsilon)));
                        if (nexty > stack[stack_idx].y) {
                            stack[stack_idx].y = nexty;
                            stack[stack_idx].x = problem.minx;
                            continue :outer;
                        } else if (@intToFloat(f64, stack[stack_idx].y - stack[i].y) > old_len * (1 + epsilon)) {
                            stack[stack_idx].y = problem.maxy;
                            continue :outer;
                        } else if (stack[stack_idx].x > stack[i].x) {
                            stack[stack_idx].x = problem.maxx;
                            continue :outer;
                        } else {
                            stack[stack_idx].x += @floatToInt(i64, std.math.floor(new_len - (old_len * (1 + epsilon))));
                            continue :outer;
                        }
                    } else if (new_len < old_len) {
                        if (stack[stack_idx].x < stack[i].x) {
                            stack[stack_idx].x = stack[i].x + stack[i].x - stack[stack_idx].x;
                            continue :outer;
                        }
                    }
                    stack[stack_idx].x += 1;
                    continue :outer;
                }
            }
        }

        // if edges intersect hole
        i = 0;
        while (i < stack_idx) : (i += 1) { // all vertices already set
            const old_len = edges_matrix[i * vertices + stack_idx];
            if (old_len > 0) { // edge exist
                // iterate over hole edges
                var from: usize = 0;
                while (from < problem.hole.len) : (from += 1) {
                    const to: usize = try std.math.mod(usize, from + 1, problem.hole.len);
                    if (intersect_really(stack[i], stack[stack_idx], problem.hole[from], problem.hole[to])) {
                        stack[stack_idx].x += 1;
                        continue :outer;
                    }
                }
            }
        }

        if (stack_idx == vertices - 1) {
            const score = calc_score(problem, stack);
            if (best_score == -1 or score < best_score) {
                best_score = score;
                std.mem.copy(Point, best_stack, stack);
                std.debug.print("score={} time={} ms vertices=[", .{ score, timer.read() / 1_000_000 });
                for (stack) |p, j| {
                    std.debug.print("[{},{}]", .{ p.x, p.y });
                    if (j < stack.len - 1) {
                        std.debug.print(", ", .{});
                    }
                }
                std.debug.print("]\n", .{});
                try save_solution(problem, stack, allocator);
            }
            stack[stack_idx].x += 1;
            continue;
        }

        stack_idx += 1;
        stack[stack_idx].x = problem.minx;
        stack[stack_idx].y = problem.miny;
    }

    if (best_score > -1) {
        try save_solution(problem, best_stack, allocator);
    }
    std.debug.print("Solved in {} ms\n", .{ timer.read() / 1_000_000 });
}
