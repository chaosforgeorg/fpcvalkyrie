OS = "WINDOWS"
dofile("lua_make.lua")

assert(os.path_normalize([[../jhc]]) == [[..\jhc]])
assert(os.path_normalize([[..\jhc]]) == [[..\jhc]])
assert(os.path_is_absolute([[D:\jhc]]))
assert(os.path_is_absolute([[\\server\jhc]]))
assert(not os.path_is_absolute([[\jhc]]))
assert(not os.path_is_absolute([[..\jhc]]))
assert(os.path_join([[D:/drl]], "bin", "data") ==
  [[D:\drl\bin\data]])

local commands = {}
local real_execute = os.execute
os.execute = function(command)
  commands[#commands + 1] = command
  return 0
end

os.execute_in_dir("drlwad ..", "bin")
assert(commands[1] == "cd bin && drlwad .. && cd ..",
  "two-argument execution must remain byte-for-byte unchanged")

os.execute_in_dir("drlwad", "bin", {
  [[D:\JHC Source\build.lua]],
})
assert(commands[2] ==
  [[cd bin && drlwad "D:\JHC Source\build.lua" && cd ..]],
  "argument-list execution must quote each argument")

os.copy_file(
  [[D:\JHC Source\setup\app_build_3126530.vdf]],
  [[D:\drl\drl-win-steam\data\jhc\setup]]
)
assert(commands[3] ==
  [[cp "D:\JHC Source\setup\app_build_3126530.vdf" "D:\drl\drl-win-steam\data\jhc\setup"]],
  "literal file copy must quote Windows source and destination paths")

OS = "LINUX"
os.path_sep = "/"
assert(os.path_normalize([[..\jhc]]) == "../jhc")
assert(os.path_is_absolute([[\jhc]]))
assert(not os.path_is_absolute([[C:\jhc]]))
assert(os.path_join("/workspace", "bin", [[..\jhc]]) ==
  "/workspace/bin/../jhc")
assert(os.quote_argument("JHC Source") == "'JHC Source'")
assert(os.quote_argument("JHC's Source") == "'JHC'\\''s Source'")
os.execute_in_dir("drlwad ../drlhq.build.lua", "bin")
assert(commands[4] == "cd bin && ./drlwad ../drlhq.build.lua && cd ..",
  "POSIX two-argument execution must remain byte-for-byte unchanged")

os.copy_file(
  "/workspace/JHC Source/setup/app_build_3126530.vdf",
  "/workspace/drl steam/data/jhc/setup"
)
assert(commands[5] ==
  "cp '/workspace/JHC Source/setup/app_build_3126530.vdf' " ..
  "'/workspace/drl steam/data/jhc/setup'",
  "literal file copy must quote POSIX source and destination paths")

local real_exit = os.exit
local exit_called = false
os.exit = function(code)
  exit_called = true
  error("unexpected os.exit(" .. tostring(code) .. ")")
end
os.execute = function(command)
  commands[#commands + 1] = command
  return 7 * 256
end
local success, message = pcall(
  os.execute_in_dir,
  "drlwad",
  "bin",
  { "../demo.build.lua" },
  true
)
assert(not success, "raise mode must report a failed command")
assert(not exit_called, "raise mode must not terminate before cleanup")
assert(message:find("Command failed with exit code 7", 1, true),
  "raise mode must report the normalized exit code")

os.execute = real_execute
os.exit = real_exit
print("lua_make path and argument tests passed")
