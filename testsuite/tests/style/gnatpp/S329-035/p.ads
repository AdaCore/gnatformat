with system.multiprocessors.dispatching_domains;
package p is
task T is
entry set (core_affinity : in natural; success : out boolean);
end;
end;
