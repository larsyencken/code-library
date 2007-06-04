#import "ConverterController.h"

@implementation ConverterController

- (IBAction)convert:(id)sender
{
	float rate, currency, amount;
	
	currency = [dollarField floatValue];
	rate = [rateField floatValue];
	amount = [converter convertCurrency:currency atRate:rate];
	
	[amountField setFloatValue:amount];
	[rateField selectText:self];
}

@end
