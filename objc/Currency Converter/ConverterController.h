/* ConverterController */

#import <Cocoa/Cocoa.h>
#import "Converter.h"

@interface ConverterController : NSObject
{
    IBOutlet NSTextField *amountField;
    IBOutlet Converter *converter;
    IBOutlet NSTextField *dollarField;
    IBOutlet NSTextField *rateField;
}
- (IBAction)convert:(id)sender;
@end
