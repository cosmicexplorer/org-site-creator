#!/usr/bin/env coffee

fs = require 'fs'
path = require 'path'
spawn = require('child_process').spawn

[jekyllServePort, emacsScript, inputDir, outputDir] = process.argv[2..]

# used because fs.watch keeps reading it as if the file was changed twice
# whenever the file is saved
recentlyModified = {}

orgFileRegex = /\.org$/

childProcs = []

removeProcFromListing = (proc) ->
  childProcs.slice childProcs.indexOf(proc), 1
setupProc = (name, args, cb) ->
  proc = spawn name, args
  childProcs.push proc
  proc.stdout.pipe process.stdout
  proc.stderr.pipe process.stderr
  proc.on 'close', (code) ->
    cb?()
    removeProcFromListing proc

removeAllFilesWildcard = (baseFilePath, opts) ->
  fs.stat baseFilePath, (err, stats) ->
    if not err
      outFileDir = path.dirname baseFilePath
      fs.readdir outFileDir, (dirErr, files) ->
        throw dirErr if dirErr
        files
          .map((file) -> path.join outFileDir, file)
          .filter((file) -> file.indexOf(baseFilePath) is 0)
          .filter((file) -> opts?.killSelf or (file isnt baseFilePath))
          .forEach (file) -> fs.unlink file, (unlinkErr) ->
            throw unlinkErr if unlinkErr
            if file is baseFilePath
              console.error "#{opts?.filePath} => #{file} removed"
            else
              console.error "extraneous file #{file} removed"

fs.watch inputDir, recursive: true, (ev, file) ->
  filePath = path.join inputDir, file
  outFilePath = path.join outputDir, file.replace(orgFileRegex, ".html")
  fs.stat filePath, (err, stats) ->
    if not err and stats.isFile()
      return if recentlyModified[filePath]
      recentlyModified[filePath] = yes
      setupProc emacsScript, [inputDir, outputDir, filePath], ->
        console.log "#{filePath} => #{outFilePath} generated"
        removeAllFilesWildcard outFilePath, { killSelf: no, filePath: filePath }
        setTimeout((-> recentlyModified[filePath] = no), 200)
    else
      removeAllFilesWildcard outFilePath, { killSelf: yes, filePath: filePath }

# quit on 'q'
process.stdin.on 'data', (str) ->
  process.exit() if str.toString().match /^\s*[qQ]/

# start jekyll serving
setupProc 'jekyll', ['serve']

# now open the browser (after a short delay)
setTimeout((->
  setupProc './open_browser_to_url.sh',
    ["http://localhost:#{jekyllServePort}"]),
  1000)

process.on 'exit', ->
  childProcs.forEach (proc) ->
    proc.kill()
