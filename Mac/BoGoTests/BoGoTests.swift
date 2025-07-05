//
//  BoGoTests.swift
//  BoGoTests
//
//  Created by Ngo Trung on 4.7.2025.
//

import Testing
@testable import BoGo

struct BoGoTests {

    @Test func example() async throws {
        let init_success = init_engine()
        #expect(init_success == 1)
        
        var output: NSString?
        let input: String = "ddoongj"
        let success = process_text(input, &output)
        
        #expect(success == 1)
        #expect(output == "động")
    }

}
