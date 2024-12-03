WITH input AS (
    SELECT
        pg_read_file('/code/inputs/day202401.txt') AS file
),
pairs AS (
    SELECT
        regexp_matches(file, '(\d+)\s+(\d+)', 'g') pair
    FROM
        input
),
lefts AS (
    SELECT
        pair[1]::int4 leftie,
        row_number() OVER (PARTITION BY NULL ORDER BY pair[1]::int4) r
    FROM pairs ORDER BY pair[1]::int4
),
rights AS (
    SELECT
        pair[2]::int4 rightie,
        row_number() OVER (PARTITION BY NULL ORDER BY pair[2]::int4) r
    FROM pairs ORDER BY pair[2]::int4
),
sortedPairs AS (
    SELECT
        leftie,
        rightie
    FROM
        lefts,
        rights
    WHERE
        lefts.r = rights.r
),
diffs AS (
    -- `@` gives the absolute value
    SELECT
        @ (leftie - rightie) delta
    FROM
        sortedPairs
),
part1 AS (
    SELECT
        'Part 1' AS part,
        sum(delta)
    FROM
        diffs
),
part2 AS (
    SELECT
        'Part 2' AS part,
        SUM(val)
    FROM (
        SELECT
            leftie * count(*) val
        FROM
            lefts,
            rights
        WHERE
            lefts.leftie = rights.rightie
        GROUP BY
            lefts.leftie) AS i
)
SELECT
    *
FROM
    part1
UNION ALL
SELECT
    *
FROM
    part2;
