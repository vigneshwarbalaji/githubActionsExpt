package com.yoco.commons.constants;

public class InternalUsage {
    private InternalUsage(){}
    public static final String FULL = "YH0D44";
    public static final String ADAPTAVANT = "91dfed2f-d29f-4302-89ee-341e9364b941";
    public static final String FULL_US_ADMIN_CONTACT_ID = "eb3abba5-9e14-45af-83a8-6e1585d3736a";

    public static final String NO_PROJECT = "No Project";
    public static final String DEFAULT_PAYROLL = "DEFAULT_PAYROLL";

    public static boolean isFullUsAccount(String accountID){
        return FULL.equalsIgnoreCase(accountID);
    }

}
