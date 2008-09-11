//
//  DictionaryController.h
//  FOKS Dictionary
//
//  Created by Lars Yencken on 4/04/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "DictionaryModel.h"

@interface DictionaryController : NSObject {
    NSString* query;
    NSMutableArray* result;
    DictionaryModel* model;
}
- (IBAction)queryDictionary:(NSSearchField*)sender;

@end
