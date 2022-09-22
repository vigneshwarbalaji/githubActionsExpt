package com.yoco.downloads.util;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.util.Map;

public class DownloadUtil {
    public static final String DECIMAL = "decimal";
    public static final String PDF_STREAM = "pdfStream";
    public static final String FILE_NAME = "fileName";

    private DownloadUtil(){}

    public static void setHeadersAndContentTypeForThePdfResponse(Map<String, Object> responseMap, HttpServletResponse response, HttpServletRequest request) throws IOException {
        String fileName = (String) responseMap.get(FILE_NAME);
        response.setHeader("Content-Disposition", "attachment; filename=\"" + fileName + "\"");
        response.setContentType("application/pdf");
        response.setHeader("Access-Control-Allow-Origin", request.getHeader("Origin"));
        response.setHeader("Access-Control-Expose-Headers","Content-Disposition");
        ByteArrayOutputStream report = (ByteArrayOutputStream)responseMap.get(PDF_STREAM);
        response.setContentLength(report.size());
        report.writeTo(response.getOutputStream());
    }

    public static String getDecimalDuration(long duration) {
        return new DecimalFormat("#0.00").format(duration / (3600000D));
    }
}
