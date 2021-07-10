#!/bin/bash
for number in {1..88}
do
curl -H "Authorization: Bearer $ICFPC2021_TOKEN" "https://poses.live/api/problems/$number" > problems_day2/$number.problem
done
