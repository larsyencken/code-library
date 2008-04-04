//
//  ConverterController.h
//  Currency Converter
//
//  Created by Lars Yencken on 4/04/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "Converter.h"

@interface ConverterController : NSObject {
    IBOutlet NSTextField *amountField;
    IBOutlet NSTextField *dollarField;
    IBOutlet NSTextField *rateField;
    Converter* converter;
}
- (IBAction)convert:(id)sender;

@end
