&fakecpu: SELECT 0.5 AS cpu_usage, to_duration("+1 00:00:00.000") AS expiry;
&name: SELECT first(1,name) AS most_cpu_usage ORDER BY cpu_usage ASC NULLS FIRST;
&kids_expiry_sum: SELECT sum(expiry) AS expiry_sum;
&nested: SELECT first(10, name) AS highest_cpu_usage WHERE cpu_usage >= (SELECT max(cpu_usage))
