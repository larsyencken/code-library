//
//  Converter.m
//  Currency Converter
//
//  Created by Lars Yencken on 4/04/08.
//  Copyright 2008 __MyCompanyName__. All rights reserved.
//

#import "Converter.h"


@implementation Converter

@synthesize sourceCurrencyAmount, rate;

- (float)convertCurrency {
    return self.sourceCurrencyAmount * self.rate;
}

@end
