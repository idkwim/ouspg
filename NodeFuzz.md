# THIS IS DOCUMENTATION DRAFT FOR NODEFUZZ #

# Getting it running #

You can download NodeFuzz either from  NodeFuzz [git-repo](https://github.com/attekett/NodeFuzz) or from [Downloads](http://code.google.com/p/ouspg/downloads/list)

Usage:

1.Install nodejs.

2.npm install in NodeFuzz root folder (Note: If you see error about native code compile from websocket-module you can ignore it. NodeFuzz doesn't use that feature of node-websocket.)

3.Configure required parameters from config.js

4.node nodefuzz.js `[arguments]` You can check the arguments available with node nodefuzz.js --help (Not many arguments available yet.)

5.see results in result-folder (default: ../results)


# NodeFuzz #

NodeFuzz is a fuzzer harness for web browsers and browser like applications. There  is two main ideas behind NodeFuzz. First one is to create a simple and fast way to fuzz different browser. Second one is to have one harness that can be easily expanded, with new test case generators and client instrumentations, without modifications for the core.

# Core #

The core of the NodeFuzz is simple HTTP- and WebSocket-server written in NodeJS. It utilizes WebSocket-server implementation from NodeJS module [WebSocket-Node](https://github.com/Worlize/WebSocket-Node). The WebSocket is used because it allows fast and continuous connection between client-software under testing and the NodeFuzz. The continuous bi-directional TCP-connection allows small overhead when test cases are sent to the client and also allows instrumentation to have information from client if needed.

# Reproducibility #

Because in some cases the malfunction of the client software can happen as a cumulative effect of multiple test cases, the NodeFuzz core has a buffer for the sent test cases. The buffer is available for instrumentation modules and the size of the buffer can be adjusted by user through configs.

# Test Case Generator Modules #

In fuzzing the test case generators are the most important part. The NodeFuzz has its own module loader, which allows user to specify either a single file or the files from a folder to be used as a test case generator(s). The module loader detects NodeFuzz compatible modules by checking if they have exported fuzz-method. The exported fuzz-method when called should return a test case in a string- or buffer-form, if module cannot return test case when requested it should return empty string. Test case generators can also export init-method, which is executed when the NodeFuzz requires the module. This feature was implemented because in some cases user wants to use their module independently from the NodeFuzz and execution of NodeFuzz related initializations is not needed or might cause errors.

### Sample: Test Case Generator ###
```
 function generateTestCase(){
   var returnString=''
   for(x=0;x<10;x++){
     returnString+='setTimeout(function(){document.body.style.zoom='+Math.random()*3+'},'+Math.floor(Math.random()*30)+')\n'
   }
   return returnString
 }
module.exports={
 fuzz:function(){
	return '<html><body><p>Hello Fuzz</p><script>'+generateTestCase()+'</script></body></html>'
 },
 init:function(){
        config.sample=true
 }
}
```

# Instrumentation Modules #

In the NodeFuzz instrumentation modules are used to control the client-software and to detect when client-software malfunctions. The instrumentation modules can be as simple as poll dmesg on linux for client crashes and save the buffer of previous test cases when crash occurs. On linux, already working instrumentations have been created for ASAN-built browsers and on Windows there is prototype instrumentation that works together with PowerShell-scripts to detect crashes and analyze them with windbg.

## Instrumentation Events ##

Instrumentation modules can take use of events emitted through eventEmitter-object "instrumentationEvents"

## Available events ##

### websocketTimeout ###

Event triggered by the Core if client fails to request new test case within time specified by config.timeout after previous request.

### startClient ###
(Not available in the current release version)

Event triggered once the Core has finished NodeFuzz initialization. (Note: This event triggers on the instant when code in the Core has been run, there is no verification if external modules have had their asynchronous code executed.)

### testCasesWithoutRestartLimit ###

Event triggered when the amount of test cases specified by config.testCasesWithoutRestart has been sent to the client without disconnect of the WebSocket connection. This event can be used to, in example restart the client to avoid extensive resource usage when using AddressSanitizer.

### feedbackMessage ###
(Not available in the current release version)

Event triggered when WebSocket message using subprotocol 'feedbackProtocol' has been received. Content of the message is delivered as UTF-8 string.

### websocketDisconnected ###

Event triggered when the WebSocket connection between the client and the Nodefuzz has been disconnected.

### Example usage ###
```
instrumentationEvents.on('startClient',startClient)

instrumentationEvents.on('websocketTimeout',restartClient)
instrumentationEvents.on('websocketDisconnected',restartClient)
instrumentationEvents.on('testCasesWithoutRestartLimit',restartClient)

instrumentationEvents.on('feedbackMessage',handleFeedback)
```


## Saving the results ##

Once crash is detected by the instrumentation, the previously sent test cases can be accessed by from config.previousTestCasesBuffer. The buffer is type Array and holds number of previous test cases specified by config.bufferSize. The buffer is basically a circular buffer where, once full, the oldest test case is removed when new one is saved.

### Sample: Save results ###
```

function saveResults(filename,prefix){
 config.previousTestCasesBuffer.forEach(function(testCaseContent,index){
   fs.writeFileSync(testCaseContent,config.result_dir+'/'+filename+index+prefix)
 })
}

```

# Known Results #

The list below has some CVE:s we know of that have been found by using NodeFuzz.

  * [CVE-2012-5108](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-5108) - Chrome - OUSPG - radamsa
  * [CVE-2012-2887](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-2887) - Chrome - OUSPG - radamsa
  * [CVE-2012-2900](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-2900) - Chrome - OUSPG
  * [CVE-2012-2876](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-2876) - Chrome - OUSPG
  * [CVE-2012-2883](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-2883) - Chrome - OUSPG
  * [CVE-2012-5120](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-5120) - Chrome - OUSPG - radamsa
  * [CVE-2012-5121](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-5121) - Chrome - OUSPG - radamsa
  * [CVE-2012-5130](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-5130) - Chrome - OUSPG
  * [CVE-2012-5145](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-5145) - Chrome - OUSPG - radamsa
  * [CVE-2013-0839](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0839) - Chrome - OUSPG
  * [CVE-2013-0879](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0879) - Chrome - OUSPG
  * [CVE-2013-0881](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0881) - Chrome - OUSPG
  * [CVE-2013-0883](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0883) - Chrome - OUSPG
  * [CVE-2013-0904](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0904) - Chrome - OUSPG
  * [CVE-2013-0905](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0905) - Chrome - OUSPG
  * [CVE-2012-4185](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-4185) - Mozilla Firefox - OUSPG
  * [CVE-2012-4186](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-4186) - Mozilla Firefox - OUSPG - radamsa
  * [CVE-2012-4187](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-4187) - Mozilla Firefox - OUSPG - radamsa
  * [CVE-2012-4188](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-4188) - Mozilla Firefox - OUSPG - radamsa
  * [CVE-2012-4202](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2012-4202) - Mozilla Firefox - OUSPG - radamsa
  * [CVE-2013-0744](http://web.nvd.nist.gov/view/vuln/detail?vulnId=CVE-2013-0744) - Mozilla Firefox - OUSPG - radamsa


We would like to thank the Chromium project and Mozilla for analyzing, fixing and reporting further many of the above mentioned issues.

# MORE DOCUMENTATION INCOMING #