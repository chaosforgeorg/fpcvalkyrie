if not OS then
	local ENV_OS     = os.getenv("WINDIR")
	local ENV_WINDIR = os.getenv("WINDIR")
	local ENV_HOME   = os.getenv("HOME")

	if ENV_OS and string.match( string.upper( ENV_OS ), "WIN" ) then
		OS = "WINDOWS"
	elseif ENV_WINDIR then
		OS = "WINDOWS"
	elseif ENV_HOME and string.match( ENV_HOME, "Users" ) then
		OS = "MACOSX"
	elseif ENV_HOME and string.match( ENV_HOME, "home" ) then
		OS = "LINUX"
	else
		error("OS not defined, and could not be autodetected!")
	end
	print ("Autodetected OS - "..OS)
end

if OS ~= "WINDOWS" and OS ~= "LINUX" and OS ~= "MACOSX" then 
	error("Unknown OS :"..OS)
end

os.os_params = {
	WINDOWS = { path_sep = "\\", short_name = "win",  exec_ext = ".exe", pack_ext = ".zip" },
	LINUX   = { path_sep = "/",  short_name = "linux",exec_ext = "", pack_ext = ".tar.gz" },
	MACOSX  = { path_sep = "/",  short_name = "osx",  exec_ext = "", pack_ext = ".tar.gz" },
}

os.path_sep   = os.os_params[ OS ].path_sep
os.exec_ext   = os.os_params[ OS ].exec_ext
os.pack_ext   = os.os_params[ OS ].pack_ext
os.short_name = os.os_params[ OS ].short_name

function os.readsingleline( filename )
	local f = assert(io.open(filename, "r"))
    local line = f:read()
    f:close()
    return line
end

function os.mkdir( list )
	if type(list) == "string" then list = { list } end
	for _,path in ipairs(list) do
		if OS == "WINDOWS" then path = path:gsub("/","\\") end
		os.execute( "mkdir "..path )
	end
end

function os.rm( list )
	if type(list) == "string" then list = { list } end
	for _,path in ipairs(list) do
		os.execute( "rm -rf "..path )
	end
end

function os.copy( from, dest )
	if type(from) == "string" then from = { from } end
	for _,path in ipairs(from) do
		os.execute( "cp "..path.." "..dest )
	end
end

function os.pwd()
	local result = ""
	if OS == "WINDOWS" then 
		result = io.popen"cd":read'*l'
	else
		result = io.popen"pwd":read'*l'
	end
	return result
end

function os.execute_in_dir( filename, dir )
	if OS == "WINDOWS" then
		os.execute("cd "..dir.." && "..filename.." && cd ..")
	else
		os.execute("cd "..dir.." && ./"..filename.." && cd ..")
	end
end

make = {}

function make.fpc( file, ... )
	local fpc_params = {
		"-FEbin",
		"-FUtmp",
		"-gl -O2",
	}

	local options = { ... }
	local option_string = " -v0 "..table.concat( fpc_params, " " )
	for _,opt in ipairs(options) do
		if type(opt) == "table"  then option_string = option_string.." "..table.concat( opt, " " ) end
		if type(opt) == "string" then option_string = option_string..opt end
	end

	local cmd = FPC_CMD or "fpc"	

	local result = os.execute(cmd.." "..file..option_string)
	if result ~= 0 then os.exit(result) end
end

function make.clean_fpc( dir )
	os.execute( "rm -f "..dir.."/*.ppu" )
	os.execute( "rm -f "..dir.."/*.o" )
	os.execute( "rm -f "..dir.."/*.exe" )
end

function make.compile( makefile )
	make.makefile = makefile or _G["makefile"]
	if not make.makefile then
		error("make structure not specified!")
		return
	end

	local tmp_dir   = make.makefile.tmp_dir or "tmp"
	local src_dir   = make.makefile.source_dir or "src"
	local bin_dir   = make.makefile.bin_dir or "bin"
	local pkg_dir   = make.makefile.pkg_dir or "pkg"
	local os_params = (make.makefile.fpc_os_params or {})[OS]

	if make.makefile.pre_build then make.makefile.pre_build() end

	make.clean_fpc( tmp_dir )
	for _,source in ipairs(make.makefile.source_files) do
		make.fpc( src_dir.."/"..source, make.makefile.fpc_params, os_params )
	end
	make.clean_fpc( tmp_dir )

	if make.makefile.post_build then make.makefile.post_build() end
end

function make.publish( name, scheme, target )
	make.makefile = make.makefile or _G["makefile"]

	local bin_dir   = make.makefile.bin_dir or "bin"
	local pkg_name  = target or (make.makefile.name.."-"..os.short_name.."-"..name)
	local publish   = make.makefile.publish
	if scheme then
		publish = publish[scheme]
	end

	if not target then
		os.rm(pkg_name)
		os.mkdir(pkg_name)
	end

	local do_copy = function ( what, dir )
		local path_from = bin_dir.."/"..( dir or "" )
		local path_to   = pkg_name.."/"..( dir or "" )
		if type(what) == "table" then
			path_from = path_from.."/"..what[1]
			path_to   = path_to.."/"..what[2]
		else
			path_from = path_from.."/"..what
		end
		os.copy( path_from, path_to )
	end

	for _,files in ipairs(publish.files or {}) do
		do_copy( files )
	end
	for _,files in ipairs(publish.exec or {}) do
		do_copy( files..os.exec_ext )
	end
	for _,files in ipairs(publish.os[ OS ] or {}) do
		do_copy( files )
	end
	for _,files in ipairs(publish.other or {}) do
		do_copy( files )
	end
	for dir,files in pairs(publish.subdirs or {}) do
		os.mkdir(pkg_name.."/"..dir)
		if type(files) == "table" then
			for _,file in ipairs(files) do
				do_copy( file, dir )
			end
		else
			do_copy( files, dir )
		end
	end
	return pkg_name
end

function make.package( dir, target )
	local filename = dir..os.pack_ext
	os.remove(filename)
	if OS == "WINDOWS" then 
		os.execute("zip -r -D "..filename.." "..dir.."/*")
	else
		os.execute("tar czf "..filename.." "..dir.."/")
	end
	os.rm( dir )
	if target then 
		os.remove(target..filename)
		os.rename(filename,target..filename)
	end
	return filename
end

function make.version_name()
	make.makefile = make.makefile or _G["makefile"]
	local ver = "trunk"
	if make.version then
		ver = table.concat( make.version.tarray )
		if make.version.beta then
			ver = ver.."-"..string.lower( string.gsub( make.version.beta, " ", "" ) )
		end
	end
	return ver
end

function make.version_human_name()
	make.makefile = make.makefile or _G["makefile"]
	local ver = "trunk"
	if make.version then
		ver = table.concat( make.version.tarray, "." )
		if make.version.beta then
			ver = ver.." "..make.version.beta
		end
	end
	return ver
end

function make.ends_with(str,ends)
	local endings = ends
	if type(ends) == "string" then
		endings = { ends }
	end
	for _,e in ipairs(endings) do
   		if e == '' or string.sub(str,-string.len(e))==e then return true end 
   	end
   	return false
end

function make.archive_name()
	return make.makefile.name.."-"..os.short_name.."-"..make.version_name()
end

function make.readversion( filename )
    local vline = os.readsingleline( filename )
    local v,maj,min,p,p2,beta = vline:match("^(%D*(%d)%.(%d+)%.(%d+)%.?(%d*)%w*%s?([%w ]*))")
    v = v:match( "(.-)[%s%c]*$" )
    local tarray = { maj, min, p }
    local array = { tonumber(maj), tonumber(min), tonumber( p ) }
    if p2 and #p2 > 0 then 
    	array[4] = tonumber(p2) 
    	tarray[4] = p2
    end
    local result = {
    	version = v,
    	array   = array,
    	tarray  = tarray,
    	major   = array[1],
    	minor   = array[2],
    	sub     = array[3],
    	patch   = array[4],
    }
    if beta and #beta > 0 then result.beta = beta end
    make.version = result
    return result
end

function make.svnrevision()
	os.execute( "svnversion . > revision.info" )
    local svnline = os.readsingleline( "revision.info" )
    local working, current, mod
    if svnline:match(":") then
    	full, working, current, mod = svnline:match("((%d+):(%d+)(%D?))")
    else
    	full, current, mod = svnline:match("((%d+)(%D?))")
    	working = current
    end
    return {
    	full    = full,
    	working = tonumber(working),
    	current = tonumber(current),
    	mod     = mod
    }
end

function make.gitrevision()
	os.execute( "git rev-parse --short HEAD > revision.info" )
	local revision = os.readsingleline( "revision.info" )

	os.execute( "git diff-index --quiet HEAD -- || echo modified > mod.info" )
	local mod = os.readsingleline( "mod.info" )

	return {
		full    = revision,
		working = tonumber(revision, 16),
		current = tonumber(revision, 16),
		mod     = mod
	}
end

function make.svncheck( data )
	if data.working ~= data.current then
		print( "Working copy not equal to HEAD! Please svn update!")
		os.exit(0)
	end
	if data.mod and #(data.mod) > 0 then
		print( "Local modifications detected! Please commit! ("..data.mod..")")
		os.exit(0)
	end
end

function make.substitute_text( text, sub_table )
	return (string.gsub(text, "({!([^}]+)})",
		function(whole,i)
			return sub_table[i] or whole
		end))
end	

function make.write_file( file, text, subs )
	if subs then text = make.substitute_text( text, subs ) end
	os.remove( file )
	local f = assert(io.open(file, "w"))
	f:write(text)
    f:close()
end

function make.writeversion( target, vdata, svndata )
	local subs = {
		string   = vdata.version,
		length   = tostring( #vdata.tarray ),
		array    = table.concat( vdata.tarray, "," ),
		beta     = "false",
		revision = svndata.current,
	}
	if vdata.beta then subs.beta = "true" end
	local text = [[// Autogenerated by FPC Valkyrie lua_make.lua
VERSION_STRING = '{!string}';
VERSION_ARRAY  : array[1..{!length}] of byte = ({!array});
VERSION_BETA   = {!beta};
VERSION_REV    = {!revision};	
]]
	make.write_file( target, text, subs )
end

function make.command( command )
	if not command then return end
	make.makefile = make.makefile or _G["makefile"]
	local mc = make.makefile.commands
	if not mc then return end
	if not mc[ command ] then
		print "Invalid command, valid commands are:"
		for k,_ in pairs(mc) do
			print( "  "..k )
		end
		return 
	end
	mc[ command ]()
end

function make.generate_iss( filename, scheme, publish_dir ) 
	make.makefile = make.makefile or _G["makefile"]
	local mi        = make.makefile.install
	if not mi then return end
	local bin_dir   = make.makefile.bin_dir or "bin"
	local pkg_dir   = make.makefile.pkg_dir or "pkg"
	local publish   = make.makefile.publish
	local out_name  = make.makefile.name.."-"..make.version_name()
	if scheme then
		publish = publish[scheme]
	end
	local exe_file  = publish.exec[1]..os.exec_ext

	filename = filename or (make.makefile.name..".iss")
	os.remove( filename )

	local f = assert(io.open(filename, "w"))
	local pwd = os.pwd()
	local subs = {}
	for k,v in pairs(mi) do
		if type(v) == "string" then subs[k] = v end
	end
	subs.name_clean    = string.gsub( subs.name, "&", "&&")
	subs.out_name      = out_name
	subs.raw_name      = make.makefile.name
	subs.version       = make.version_human_name()
	subs.version_fixed = make.version_name()
	subs.exe           = exe_file
	subs.root          = pwd.."\\"
	subs.bin           = pwd.."\\"..bin_dir.."\\"
	subs.output_dir    = publish_dir or (pwd.."\\"..pkg_dir)
	local text = [[; Script generated by FPC Valkyrie lua_make.lua
; Do not edit!

[Setup]
AppId={{{!guid}}
AppName={!name}
AppVersion={!version}
;AppVerName={!name} {!version}
AppPublisher={!publisher}
AppPublisherURL={!iss_url}
AppSupportURL={!iss_url}
AppUpdatesURL={!iss_url}
DefaultDirName={pf}\{!name}
DefaultGroupName={!name}
LicenseFile={!root}{!license}
InfoAfterFile={!root}{!info_after}
OutputDir={!output_dir}
OutputBaseFilename={!out_name}
SetupIconFile={!root}{!iss_icon}
Compression=lzma2/max
SolidCompression=yes
WizardImageBackColor=clBlack
WizardImageFile={!root}{!iss_image}
WizardSmallImageFile={!root}{!iss_simage}

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked; OnlyBelowVersion: 0,6.1

[Dirs]
Name: "{app}\"; Permissions: everyone-modify

[Files]
]]
	f:write( make.substitute_text( text, subs ) )

	local emit_files = function ( what, dir )
		local path_from = pwd.."\\"..bin_dir
		local path_to   = "{app}"
		local dest_name = ""
		if dir then 
			path_from = path_from.."\\"..dir
			path_to = path_to.."\\"..dir
		end
		if type(what) == "table" then
			path_from = path_from.."\\"..what[1]
			dest_name = "DestName: \""..what[2].."\"; "
		else
			path_from = path_from.."\\"..what
		end
		local compression = ""
		if mi.iss_nocomp then 
			if make.ends_with(path_from,mi.iss_nocomp) then compression = " nocompression" end
		end
		f:write("Source: \""..path_from.."\"; "..dest_name.." DestDir: \""..path_to.."\"; Flags: ignoreversion"..compression.."\n" )
	end

	for _,files in ipairs(publish.exec or {}) do
		emit_files( files..os.exec_ext )
	end
	for _,files in ipairs(publish.files or {}) do
		emit_files( files )
	end
	for _,files in ipairs(publish.os[ OS ] or {}) do
		emit_files( files )
	end
	for _,files in ipairs(publish.other or {}) do
		emit_files( files )
	end
	for dir,files in pairs(publish.subdirs or {}) do
		if type(files) == "table" then
			for _,file in ipairs(files) do
				emit_files( file, dir )
			end
		else
			emit_files( files, dir )
		end
	end

	local emit_icon = function ( what )
		f:write( "Name: \"{group}\\"..what.name.."\"; ")
		if what.exe then 
			local parameters = ""
			if what.parameters then 
				parameters = "; Parameters: \""..what.parameters.."\""
			end
			f:write( "WorkingDir: \"{app}\"; Filename: \"{app}\\"..what.exe..os.exec_ext.."\""..parameters.."\n" )
		elseif what.file then
			f:write( "Filename: \"{app}\\"..what.file.."\"\n" )
		elseif what.url then
			f:write( "Filename: \""..what.url.."\"\n" )
		end
	end

	f:write("\n[Icons]\n")
	for _,v in ipairs( mi.iss_eicons ) do
		emit_icon( v )
	end

	local text = [[Name: "{group}\{cm:UninstallProgram,{!name}}"; Filename: "{uninstallexe}"
Name: "{commondesktop}\{!name}"; Filename: "{app}\{!exe}"; Tasks: desktopicon
Name: "{userappdata}\Microsoft\Internet Explorer\Quick Launch\{!name}"; Filename: "{app}\{!exe}"; Tasks: quicklaunchicon

[Run]
Filename: "{app}\{!exe}"; Description: "{cm:LaunchProgram,{!name_clean}}"; Flags: nowait postinstall skipifsilent

]]
	f:write( make.substitute_text( text, subs ) )
	f:close()

	os.execute( "iscc \""..filename.."\"" )
	return filename
end

function make.generate_bundle( scheme, publish_dir ) 
	make.makefile = make.makefile or _G["makefile"]
	local mi        = make.makefile.install
	if not mi then return end
	local pkg_dir   = make.makefile.pkg_dir or "pkg"
	local publish   = make.makefile.publish
	if scheme then
		publish = publish[scheme]
	end

	local app_name     = mi.name
	local app_folder   = app_name..".app"
	local macos_folder = app_folder.."/Contents/MacOS"
	local res_folder   = app_folder.."/Contents/Resources"
	local fwork_folder = app_folder.."/Contents/Frameworks"
	local plist_file   = app_folder.."/Contents/Info.plist"
	local pkginfo_file = app_folder.."/Contents/PkgInfo"
	local dmg_folder   = "dmg"
	local app_file     = publish.exec[1]
	local app_root     = make.makefile.bin_dir or "bin"
	local target_dmg   = make.makefile.name.."-"..make.version_name()..".dmg"
	local temp_dmg     = "pack.temp.dmg"
	local volume_name  = app_name.." "..make.version_human_name()
	local pkginfo_text = "APPLMAG#"

	os.rm( app_folder )
  os.mkdir( app_folder )
	os.mkdir( app_folder.."/Contents" )
	os.mkdir( macos_folder )
	os.mkdir( res_folder )
	os.mkdir( fwork_folder )

	local temp_exec = publish.exec
	publish.exec = nil
	make.publish( "", scheme, res_folder )
	publish.exec = temp_exec

	if mi.app_icon   then os.copy( mi.app_icon, res_folder ) end
	if mi.app_fworks then 
		for _,v in ipairs( mi.app_fworks ) do
			os.copy( "-R "..v, fwork_folder ) 
		end
	end

	os.copy( app_root.."/"..publish.exec[1], macos_folder.."/"..app_name )
	if mi.app_exefix then mi.app_exefix( macos_folder.."/"..app_name ) end

	local text = [[<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>CFBundleDevelopmentRegion</key>
  <string>English</string>
  <key>CFBundleExecutable</key>
  <string>]]..app_name..[[</string>
  <key>CFBundleIconFile</key>
  <string>iconfile.icns</string>
  <key>CFBundleIdentifier</key>
  <string>org.magnifier.magnifier</string>
  <key>CFBundleInfoDictionaryVersion</key>
  <string>6.0</string>
  <key>CFBundlePackageType</key>
  <string>APPL</string>
  <key>CFBundleSignature</key>
  <string>MAG#</string>
  <key>CFBundleVersion</key>
  <string>1.0</string>
</dict>
</plist>
]]
	make.write_file( plist_file, text )
	make.write_file( pkginfo_file, pkginfo_text )

	local subs = {}
	for k,v in pairs(mi) do
		if type(v) == "string" then subs[k] = v end
	end
	subs.volume_name = volume_name
	subs.app_folder  = app_folder

	os.rm( dmg_folder )
	os.mkdir( dmg_folder )
	os.mkdir( dmg_folder.."/.background" )
	os.copy( mi.app_bg, dmg_folder.."/.background/background.png" )
  os.execute( "ln -sf /Applications "..dmg_folder.."/Applications" )
  os.rename( app_folder, dmg_folder.."/"..app_folder )

	local text = [[
tell application "Finder"
  tell disk "{!volume_name}"
    open
    set current view of container window to icon view
    set toolbar visible of container window to false
    set statusbar visible of container window to false
    set the bounds of container window to {100, 100, 865, 680}
    set theViewOptions to the icon view options of container window
    set arrangement of theViewOptions to not arranged
    set icon size of theViewOptions to 128
    set background picture of theViewOptions to file ".background:background.png"
    set position of item "{!app_folder}" of container window to {230, 400}
    set position of item "Applications" of container window to {560, 400}
    close
    open
    update without registering applications
    delay 5
  end tell
end tell	
]]
	make.write_file( "dmg_setup.as", text, subs )

	os.execute('hdiutil create -srcfolder "'..dmg_folder..'" -volname "'..volume_name..'" -fs HFS+ -fsargs "-c c=64,a=16,e=16" -format UDRW -size '..tostring(mi.dmg_size)..'k '..temp_dmg )
	os.execute('hdiutil attach -readwrite -noverify -noautoopen "'..temp_dmg..'"')
	os.execute('sleep 5')
	os.execute('osascript dmg_setup.as')
	os.execute('chmod -Rf go-w "/Volumes/'..volume_name..'"')
	os.execute('sync')
	os.execute('sync')
	os.execute('hdiutil eject "/Volumes/'..volume_name..'"')
	os.remove( target_dmg )
	os.execute('hdiutil convert "'..temp_dmg..'" -format UDZO -imagekey zib-level=9 -o '..target_dmg )
	os.remove( temp_dmg )
	if publish_dir then
		os.rename( target_dmg, publish_dir..target_dmg )
	end
end

