# Introduction #

http://fuzz.eventbrite.com/

Slides can be downloaded from: https://code.google.com/p/ouspg/downloads/detail?name=slides.pdf

# Details #
  * Also check [Radamsa](Radamsa.md) and [Blab](Blab.md) wiki pages for more information.


---


Example: record some data using tcpflow (browse a few websites after starting the capture). The traffic is written as files to the current folder. Serve.sh tells radamsa to fuzz and serve the server responses (=flow captures originating from port 80) using port 80, so it they be accessed by navigating to the localhost with a normal web browser.

```
root@debian:/home/secpelle/flowcap# tcpflow
tcpflow[13829]: listening on eth0
^Ctcpflow[13829]: terminating
root@debian:/home/secpelle/flowcap# ls
002.020.181.016.00080-010.000.002.015.55622  010.000.002.015.60911-199.027.076.193.00080
-snip-
010.000.002.015.60235-178.236.004.192.00080  217.118.027.135.00080-010.000.002.015.55051
010.000.002.015.60236-178.236.004.192.00080  serve.sh
root@debian:/home/secpelle/flowcap# cat serve.sh
radamsa -o :80 -n inf *080-*
```

An example where data is piped to radamsa. Additionally, -n flag is used to get out five cases.

```
[user:~] $ echo "Fuzz is the new buzz." | radamsa -n 5
Fuzz i󠁉s the new buzz.
FFuzz is the new buzz.
FuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzFuuzzFuuzzFuuzzFuzzzzFuuzzFuuzzFuuzzFuuzzFuuzzFuuzzFuuzzFuuzzFuuzzFuuzzFuuzz iFuzz isFuzz is theFuzz is the neFuzz is is the new buzzz.
Fuzz is the new buzz.
Fuzz is th󠁁?e new    buzz.
FuzzFuuzzFuuzzFuuzzFuuzz.
Fuzz is the new buzz.
[user:~] $ 
```

Create five fuzzed files using the file "fuzz.txt" as the basis.

```
[user:~] $ echo "Fuzz is the new buzz." > fuzz.txt
[user:~] $ radamsa -n 5 -o fuzz-%n.txt fuzz.txt
[mutjake:~] $ ls fuzz-*.txt
fuzz-1.txt fuzz-2.txt fuzz-3.txt fuzz-4.txt fuzz-5.txt
```

Create five more files using the files created previously as the basis.

```
[mutjake:~] $ radamsa -n 5 -o fuzz--%n.txt fuzz-*.txt
[mutjake:~] $ ls fuzz--*.txt
fuzz--1.txt fuzz--2.txt fuzz--3.txt fuzz--4.txt fuzz--5.txt
```

Simple Windows testing script:

```
$MutationsFolder = "C:\Users\Administrator\Documents\PdfTest\mutations"
$ApplicationPath = "C:\Users\Administrator\Documents\PdfTest\SumatraPDF\SumatraPDF.exe"
$ProcessName = "SumatraPDF"
$SampleFolder = "E:\"
$SampleFileExtension = "pdf"
$RadamsaPath = "C:\Users\Administrator\Documents\PdfTest\radamsa-0.3.exe"
 
$continue = $true
while ($continue) {
 
    # Discard old samples
   Remove-Item ($MutationsFolder + "\*." + $SampleFileExtension)
    # Create new samples. %n is markup detected by radamsa and is substituted by an index number
   $radamsastring = $RadamsaPath + " -n 100 --output " +
        $MutationsFolder + "\%n." + $SampleFileExtension +
        " " + $SampleFolder + "\*." + $SampleFileExtension
   
   iex "& $radamsastring" # Execute $radamsastring
   
   # loop through all the files in the mutations folder
   foreach($sample in Get-ChildItem $MutationsFolder -name) {
   
        # The execution string that runs the program e.g. notepad.exe mutations\sample1.txt
        $execstring = $ApplicationPath + " " + $MutationsFolder + "\" + $sample
       
        # Get timestamp with Get-Date and change it's format to the one used by the logs.
        $timestamp = [System.Management.ManagementDateTimeConverter]::ToDMTFDateTime((Get-Date))
       
        iex "& $execstring" # execute the program with the sample
       
        Start-Sleep -m 1000 # Sleep some milliseconds to give time for the software to handle the case
       
        # query string to get the list of crashes by our program, which happened after the timestamp.
        $query = "select * from win32_ntlogevent where logfile='$application'
       and eventcode='1000' and sourcename='Application Error' and
       TimeGenerated >= '$timestamp'"
       
        # do the query
        $crashlist = Get-WmiObject -Query $query
       
        # if crashes found, stop iterating and print the name of the faulting test case
        if($crashlist.length -gt 0) {
            $continue = $false
            Write-Host ("Crashed: " + $sample)
        }
        get-process $ProcessName | stop-process
 
       
        Start-Sleep -m 10 # Sleep to catch Ctrl-C.
    }
}
```

The simple Linux testing script from radamsa wiki:

```
 $ echo "1 + 2" > sample-1
 $ echo "(124 % 7) ^ 1*2" > sample-2
 $ echo "sqrt((1 + length(10^4)) * 5)" > sample-3
 $ bc sample-* < /dev/null
 3
 10
 5
 $ while true
 do
   radamsa -o fuzz-%n -n 100 sample-*
   bc fuzz-* < /dev/null
   test $? -gt 127 && break
 done
```

Sample script written in python, which creates fuzzed files to given folder and prints them:

```
import subprocess
from os import listdir
from os.path import isfile, join

sample_path = "/Users/mutjake/temp/samples"
mutation_path = "/Users/mutjake/temp/mutations"
file_extension = ".txt"
radamsa_path = "radamsa"

exec_string = radamsa_path + ' -o ' + join(mutation_path, "mutation-%n" + file_extension) + ' -n 100 ' + join(sample_path, "*" + file_extension)

print(exec_string)
subprocess.Popen(exec_string, shell=True)

mutations = [ f for f in listdir(mutation_path) if isfile(join(mutation_path,f)) ]

for mutation in mutations:
	with open(join(mutation_path, mutation), "r") as fh:
		print(fh.read())
```