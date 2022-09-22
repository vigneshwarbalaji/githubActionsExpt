package com.yoco.account.enums;

import lombok.NoArgsConstructor;

@NoArgsConstructor
public enum ACCOUNT_DISPLAY_TIMEFORMAT {

    HH_MM_SS("HH MM SS"),
    HH_MM("HH MM"),
    DECIMAL("Decimal");

    private String timeFormat;

    ACCOUNT_DISPLAY_TIMEFORMAT(String timeFormat) {
        this.timeFormat = timeFormat;
    }

    public String value() {
        return timeFormat;
    }
}
