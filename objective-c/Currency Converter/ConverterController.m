//
//  ConverterController.m
//  Currency Converter
//
//  Created by Lars Yencken on 4/04/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "ConverterController.h"


@implementation ConverterController

- (IBAction)convert:(id)sender {
    float amount;
    converter = [[Converter alloc]init];
    [converter setSourceCurrencyAmount:[dollarField floatValue]];
    [converter setRate:[rateField floatValue]];
    amount = [converter convertCurrency];
    
    [amountField setFloatValue:amount];
    [rateField selectText:self];
}

@end
