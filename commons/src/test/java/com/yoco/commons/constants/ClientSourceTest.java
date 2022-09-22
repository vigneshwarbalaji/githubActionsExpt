package com.yoco.commons.constants;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.util.HashMap;
import java.util.Map;

class ClientSourceTest {

    @Test
    void getClientIdMap_staging_test(){
        ClientSource.setClientIdMap(false);
        Map<String, String> response = ClientSource.getClientIdMap();

        Map<String,String> expectedResponse = new HashMap<>();
        expectedResponse.put("cae2f-e425095475e037698deb", ClientSource.CLIENT_SOURCE_CONSTANTS.IOS.value());
        expectedResponse.put("2a9ac-b356e8d90f6ba8f45ac26cab698b831d", ClientSource.CLIENT_SOURCE_CONSTANTS.ANDROID.value());
        expectedResponse.put("cae2f-baabe0d18f209ba05cea", ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
        expectedResponse.put("bf60d-939a41b5a49c306e70ac", ClientSource.CLIENT_SOURCE_CONSTANTS.AW.value());
        expectedResponse.put("7a0ad-aba9ec07c3515d5754ead9c399523be6", ClientSource.CLIENT_SOURCE_CONSTANTS.AW.value());
        expectedResponse.put("sa9277763248c1a4432037c44992d4c45d", ClientSource.CLIENT_SOURCE_CONSTANTS.AW.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.ZENDESK.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.ZENDESK.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.CHROME_EX.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.CHROME_EX.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.NFC_RFID.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.NFC_RFID.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.FULL_CLIENT.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.FULL_CLIENT.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.ASANA.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.ASANA.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.SCHEDULER.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.SCHEDULER.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.FIREFOX_EX.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.FIREFOX_EX.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.ONE_TAP.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.ONE_TAP.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.YOCO_CRON.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.YOCO_CRON.value());
        Assertions.assertEquals(expectedResponse,response);
    }

    @Test
    void getClientIdMap_live_test(){
        ClientSource.setClientIdMap(true);
        Map<String, String> response = ClientSource.getClientIdMap();

        Map<String,String> expectedResponse = new HashMap<>();
        expectedResponse.put("sa1a4593e99fff76b313e271e64820e5ad", ClientSource.CLIENT_SOURCE_CONSTANTS.AW.value());
        expectedResponse.put("7a0ad-6a8c594c8b47031c088ce3194a6f87fe", ClientSource.CLIENT_SOURCE_CONSTANTS.AW.value());
        expectedResponse.put("29354-222708a009322d23f1f32e91e6b2ee1c", ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
        expectedResponse.put("29354-8aa91014c836bbf6033280bcf1d38493", ClientSource.CLIENT_SOURCE_CONSTANTS.IOS.value());
        expectedResponse.put("29354-a4cac0f62c615e600babe8a74704493a", ClientSource.CLIENT_SOURCE_CONSTANTS.ANDROID.value());
        expectedResponse.put("aa8a2-6e40ddfca9136ece8007", ClientSource.CLIENT_SOURCE_CONSTANTS.WEB.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.TRELLO.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.ZENDESK.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.ZENDESK.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.CHROME_EX.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.CHROME_EX.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.NFC_RFID.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.NFC_RFID.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.FULL_CLIENT.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.FULL_CLIENT.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.ASANA.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.ASANA.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.SCHEDULER.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.SCHEDULER.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.FIREFOX_EX.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.FIREFOX_EX.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.ONE_TAP.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.ONE_TAP.value());
        expectedResponse.put(ClientSource.CLIENT_SOURCE_CONSTANTS.YOCO_CRON.value(), ClientSource.CLIENT_SOURCE_CONSTANTS.YOCO_CRON.value());
        Assertions.assertEquals(expectedResponse,response);
    }
}
