package com.yoco.commons.constants;

import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.GaeUtils;

public class SchedulingEngineUrlConstants {
    private SchedulingEngineUrlConstants(){}

    public static final AppMode APP_MODE = GaeUtils.getAppMode();
    private static String baseUrl = "";
    private static String schedulingEngineEventUrl = "";
    static{
        initializeSchedulingEngineConstants(APP_MODE);
    }

    public static void initializeSchedulingEngineConstants(AppMode appMode){
        if(AppMode.LIVE.equals(appMode)){
            setupSchedulingEngineConstantsForLive();
        }else{
            setupSchedulingEngineConstantsForStaging();
        }
        setupSchedulingEngineUrls();
    }

    public static void setupSchedulingEngineConstantsForLive(){
        baseUrl = "https://events-dot-schedulingengine.uc.r.appspot.com/";
    }

    public static void setupSchedulingEngineConstantsForStaging(){
        baseUrl = "https://events-dot-staging-schedulingengine.appspot.com/";
    }

    public static void setupSchedulingEngineUrls(){
        schedulingEngineEventUrl = baseUrl + "api/v1/events";
    }

    public static String getSchedulingEngineEventUrl(){
        return schedulingEngineEventUrl;
    }

}
