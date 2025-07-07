//
//  IMKitSampleInputController.swift
//  BoGo
//
//  Created by Ngo Trung on 4.7.2025.
//

import Cocoa
import InputMethodKit

@objc(IMKitSampleInputController)
class IMKitSampleInputController: IMKInputController {
    var unprocessed_text = ""
    var sent_text = ""
    
    override init!(server: IMKServer!, delegate: Any!, client: Any!) {
        super.init(server: server, delegate: delegate, client: client)
        NSLog("✅ MyInputController initialized with client: \(client!)")
        

        var processed_text: NSString?
        NSLog("init processing text")
        let success = process_text("ddaayj", &processed_text)
        
        if (success != 1) {
            NSLog("not success")
        }
        
        NSLog("init Success = \(success) result=\(processed_text)")
    }

    override func activateServer(_ sender: Any!) {
        NSLog("🟢 IME activated")
    }

    override func deactivateServer(_ sender: Any!) {
        NSLog("🔴 IME deactivated")
    }
    
    override func didCommand(by aSelector: Selector!, client sender: Any!) -> Bool {
        unprocessed_text = ""
        sent_text = ""
        return false
    }
    
    override func cancelComposition() {
        NSLog("cancelComposition")
    }
    
    override func commitComposition(_ sender: Any!) {
        NSLog("commitComposition")
    }
    
    override func recognizedEvents(_ sender: Any!) -> Int {
        NSLog("recognizedEvents")
        return super.recognizedEvents(sender)
    }
    
    override func inputText(_ string: String!, client sender: Any!) -> Bool {
        // get client to insert
        guard let client = sender as? IMKTextInput else {
            NSLog("Client is not IMKTextInput");
            return false
        }
        
        if (string == " ") {
            unprocessed_text = ""
            sent_text = ""
            return false
        }
        
        unprocessed_text.append(string)
        var processed_text: NSString?
//        NSLog("processing thoeutext")
        let success = process_text(unprocessed_text, &processed_text)

        if (success != 1) {
            NSLog("not success")
            return false
        }

        let replacement_len = sent_text.count
        
        client.insertText(processed_text!,
                          replacementRange: NSRange(location: client.selectedRange().location - replacement_len, length: replacement_len))
        sent_text = processed_text! as String
//        NSLog("committed \(sent_text)")
        
        return true
        
    }
}
