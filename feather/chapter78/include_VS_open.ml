module M = struct
  let x = 0;
end

module N = struct
  (* does export definitions in M *)
  include M
  let y = x + 1
end

module O = struct
  (* doesn't export definitons in M*)
  open M
  let y = x + 1
end