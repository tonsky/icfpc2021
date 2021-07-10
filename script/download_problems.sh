#!/bin/bash
for number in {1..78}
do
curl -H "Authorization: Bearer $ICFPC2021_TOKEN" "https://poses.live/api/problems/$number" > problems/$number.problem
done
