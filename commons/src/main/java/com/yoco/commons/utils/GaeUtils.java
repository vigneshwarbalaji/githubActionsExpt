package com.yoco.commons.utils;

import com.yoco.commons.enums.AppMode;

public final class GaeUtils {

    private GaeUtils(){}

    public static final String APP_ID_STAGING = "staging-goclockin-dashboard";
    public static final String APP_ID_LIVE = "live-goclockin-dashboard";

    public static String getAppId() {
        return System.getenv("GAE_APPLICATION");
    }

    public static AppMode getAppMode() {

        String appId = getAppId();

        if(ObjUtils.isNull(appId)){
            return AppMode.DEV;
        }else{

            if(appId.contains(APP_ID_LIVE))
                return AppMode.LIVE;
            else if(appId.contains(APP_ID_STAGING))
                return AppMode.STAGING;
            else
                return AppMode.DEV;
        }
    }

    public static boolean isAppModeLive(){
        return AppMode.LIVE.equals(getAppMode());
    }

}
