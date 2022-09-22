package com.yoco.downloads.util;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

class DownloadUtilTest {
    @Test
    void setHeadersAndContentTypeForThePdfResponse_valid_test() throws IOException {
        ByteArrayOutputStream report = new ByteArrayOutputStream();
        Map<String, Object>map = new HashMap<>();
        map.put(DownloadUtil.FILE_NAME, "newFile");
        map.put(DownloadUtil.PDF_STREAM, report);
        HttpServletRequest request = new MockHttpServletRequest();
        HttpServletResponse response = new MockHttpServletResponse();
        DownloadUtil.setHeadersAndContentTypeForThePdfResponse(map, response, request);
        Assertions.assertEquals("application/pdf",response.getContentType());
    }

    @Test
    void getDecimalDuration_valid_test() {
        Assertions.assertEquals("0.00",DownloadUtil.getDecimalDuration(1234L));
    }
}
