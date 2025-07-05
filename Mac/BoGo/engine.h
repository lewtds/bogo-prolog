//
//  engine.h
//  BoGo
//
//  Created by Ngo Trung on 5.7.2025.
//

#ifndef engine_h
#define engine_h

#import <Foundation/Foundation.h>


int init_engine(void);
void stop_engine(int rval);
int process_text(NSString* input, NSString **output);

#endif /* engine_h */
