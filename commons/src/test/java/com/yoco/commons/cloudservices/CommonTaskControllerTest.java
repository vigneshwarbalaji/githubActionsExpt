package com.yoco.commons.cloudservices;

import com.yoco.commons.utils.events.ClockTaskUtil;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.DcmConstants;
import com.yoco.commons.services.UrlFetcher;

import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectOutputStream;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class CommonTaskControllerTest {

    @Test
    void createClockTaskHandler_valid_test() throws IOException, ClassNotFoundException {

        try (MockedStatic<ClockTaskUtil> clockTaskUtilMockedStatic = Mockito.mockStatic(ClockTaskUtil.class)) {
            ClockTaskUtil clockTaskUtil = Mockito.mock(ClockTaskUtil.class);
            Mockito.doNothing().when(clockTaskUtil).clockOperationsTaskHandler(anyMap());
            clockTaskUtilMockedStatic.when(ClockTaskUtil::getClockTaskUtil).thenReturn(clockTaskUtil);

            Map<String, Object> payload = new HashMap<>();
            payload.put("key", "data");
            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(payload);
            new CommonTaskController().createClockTaskHandler(byteOut.toByteArray());
            Mockito.verify(clockTaskUtil).clockOperationsTaskHandler(payload);

        }
    }

    @Test
    void clearCacheTaskHandler_valid_test() throws IOException, ClassNotFoundException {
        try (MockedStatic<UrlFetcher> urlFetcherMockedStatic = Mockito.mockStatic(UrlFetcher.class)) {
            CommonAppProperties.setYoCoDefaultApiUrl("url");
            Map<String, Object> payload = new HashMap<>();
            payload.put("key", "data");
            var byteOut = new ByteArrayOutputStream();
            var out = new ObjectOutputStream(byteOut);
            out.writeObject(Map.of("token","tokenStr","payloadMap",payload));
            new CommonTaskController().clearCacheTaskHandler(byteOut.toByteArray());
            urlFetcherMockedStatic.verify(() -> UrlFetcher.sendPutRequest(eq("url/api/v2/contact/clearTokenFromCache"),eq(payload),eq(new String[]{"Content-Type","application/json","Authorization","Bearer tokenStr"}),isNull()));
        }
    }

}
