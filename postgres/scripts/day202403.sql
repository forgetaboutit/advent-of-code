WITH input AS (
    SELECT
        pg_read_file('/code/inputs/day202403.txt') AS file
),
-- remove all `don't()` instructions up to `do()` instructions or EOF
withoutDonts AS (
    SELECT
        regexp_replace(file, 'don''t\(\)(.|\n)*?(?=do\(\)|$)', '', 'g') AS file
    FROM
        input
),
mulInstructions AS (
    SELECT
        part,
        regexp_matches(file, 'mul\((\d+),(\d+)\)', 'g') AS op
    FROM (
        SELECT
            'Part 1' AS part,
            *
        FROM
            input
        UNION ALL
        SELECT
            'Part 2' AS part,
            *
        FROM
            withoutDonts) combined
)
SELECT
    part,
    sum(op[1]::int4 * op[2]::int4)
FROM
    mulInstructions
GROUP BY
    part;

