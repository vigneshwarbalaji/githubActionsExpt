package com.yoco.commons.constants;

import lombok.NoArgsConstructor;

import java.util.HashMap;
import java.util.Map;

public class ClientSource {

    private ClientSource(){}

    @NoArgsConstructor
    public enum CLIENT_SOURCE_CONSTANTS {

        IOS("ios"),
        ANDROID("android"),
        WEB("web"),
        AW("aw"),
        NFC_RFID("nfc_rfid"),
        FULL_CLIENT("FullClient"),
        YOCO_CRON("YoCo-Cron"),
        TRELLO("trello"),
        ASANA("asana"),
        ZENDESK("zendesk"),
        CHROME_EX("chrome-ex"),
        FIREFOX_EX("firefox-ex"),
        SCHEDULER("scheduler"),
        ONE_TAP("onetap"),
        HOOK("hook");

        private String value;

        CLIENT_SOURCE_CONSTANTS(String value) {
            this.value = value;
        }

        public String value() {
            return value;
        }
    }

    private static Map<String,String> clientIdMap;

    public static void setClientIdMap(boolean isLiveEnv){

        if(isLiveEnv){
            clientIdMap  = new HashMap<>();
            clientIdMap.put("sa1a4593e99fff76b313e271e64820e5ad", CLIENT_SOURCE_CONSTANTS.AW.value());
            clientIdMap.put("7a0ad-6a8c594c8b47031c088ce3194a6f87fe", CLIENT_SOURCE_CONSTANTS.AW.value());
            clientIdMap.put("29354-222708a009322d23f1f32e91e6b2ee1c", CLIENT_SOURCE_CONSTANTS.WEB.value());
            clientIdMap.put("29354-8aa91014c836bbf6033280bcf1d38493", CLIENT_SOURCE_CONSTANTS.IOS.value());
            clientIdMap.put("29354-a4cac0f62c615e600babe8a74704493a", CLIENT_SOURCE_CONSTANTS.ANDROID.value());
            clientIdMap.put("aa8a2-6e40ddfca9136ece8007",CLIENT_SOURCE_CONSTANTS.WEB.value());//zapier
        }else{
            clientIdMap  = new HashMap<>();
            clientIdMap.put("cae2f-e425095475e037698deb", CLIENT_SOURCE_CONSTANTS.IOS.value());
            clientIdMap.put("2a9ac-b356e8d90f6ba8f45ac26cab698b831d", CLIENT_SOURCE_CONSTANTS.ANDROID.value());
            clientIdMap.put("cae2f-baabe0d18f209ba05cea", CLIENT_SOURCE_CONSTANTS.WEB.value());
            clientIdMap.put("bf60d-939a41b5a49c306e70ac", CLIENT_SOURCE_CONSTANTS.AW.value());
            clientIdMap.put("7a0ad-aba9ec07c3515d5754ead9c399523be6", CLIENT_SOURCE_CONSTANTS.AW.value());
            clientIdMap.put("sa9277763248c1a4432037c44992d4c45d", CLIENT_SOURCE_CONSTANTS.AW.value());
        }

         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.TRELLO.value(),CLIENT_SOURCE_CONSTANTS.TRELLO.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.ZENDESK.value(), CLIENT_SOURCE_CONSTANTS.ZENDESK.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.CHROME_EX.value(),CLIENT_SOURCE_CONSTANTS.CHROME_EX.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.NFC_RFID.value(),CLIENT_SOURCE_CONSTANTS.NFC_RFID.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.FULL_CLIENT.value(),CLIENT_SOURCE_CONSTANTS.FULL_CLIENT.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.ASANA.value(), CLIENT_SOURCE_CONSTANTS.ASANA.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.SCHEDULER.value(),CLIENT_SOURCE_CONSTANTS.SCHEDULER.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.FIREFOX_EX.value(), CLIENT_SOURCE_CONSTANTS.FIREFOX_EX.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.ONE_TAP.value(), CLIENT_SOURCE_CONSTANTS.ONE_TAP.value());
         clientIdMap.put(CLIENT_SOURCE_CONSTANTS.YOCO_CRON.value(),CLIENT_SOURCE_CONSTANTS.YOCO_CRON.value());
    }

    public static Map<String, String> getClientIdMap() {
        return clientIdMap;
    }

}
