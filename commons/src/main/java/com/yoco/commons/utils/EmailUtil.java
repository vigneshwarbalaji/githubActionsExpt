package com.yoco.commons.utils;

public class EmailUtil {
    private EmailUtil() {}

    public static final String MAIL_SENDER = "support@yocoboard.com";
    public static final String MAIL_SENDER_NAME = "YoCoBoard";

    public static String getSubject(String subject){
        if (GaeUtils.isAppModeLive())
            return subject;
        else
            return subject.concat(" (" + GaeUtils.APP_ID_STAGING + ") ");
    }

}
