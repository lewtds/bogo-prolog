//
//  AppDelegate.swift
//  BoGo
//
//  Created by Ngo Trung on 4.7.2025.
//

import Cocoa
import InputMethodKit

class AppDelegate: NSObject, NSApplicationDelegate {
    var server = IMKServer()
    var candidatesWindow = IMKCandidates()

    func applicationDidFinishLaunching(_ notification: Notification) {
        // Insert code here to initialize your application
        
        NSLog("yo trying to create the IMKServer")
        NSLog("Bundle identifier is " + Bundle.main.bundleIdentifier!)
        server = IMKServer(name: "Bogo_1_Connection", bundleIdentifier: Bundle.main.bundleIdentifier)
        candidatesWindow = IMKCandidates(server: server, panelType: kIMKSingleRowSteppingCandidatePanel, styleType: kIMKMain)
        NSLog("tried connection")
        
        NSLog("before init")
        NSLog("Init success \(init_engine())")
        
        var processed_text: NSString?
        NSLog("processing text")
        let success = process_text("nooix", &processed_text)
        NSLog("Success = \(success) result=\(processed_text)")
    }

    func applicationWillTerminate(_ notification: Notification) {
        stop_engine(0)
    }
}

// Create and run the application
let app = NSApplication.shared
let delegate = AppDelegate()
app.delegate = delegate
app.run()
