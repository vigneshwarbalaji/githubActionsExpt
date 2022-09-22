package com.yoco.downloads.adjustment.endpoint;

import com.yoco.commons.annotation.helper.AnnotationHelper;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.constants.EventConstants;
import com.yoco.downloads.adjustment.service.AdjustmentDownloadService;
import com.yoco.downloads.util.DownloadUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.MockedStatic;
import org.mockito.Mockito;
import org.springframework.http.ResponseEntity;
import org.springframework.mock.web.MockHttpServletRequest;
import org.springframework.mock.web.MockHttpServletResponse;

import javax.servlet.http.HttpServletRequest;
import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;

class AdjustmentDownloadApiTest {
    @Test
    void downloadPdf_exception_test() {
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AdjustmentDownloadService> adjustmentDownloadServiceMock = Mockito.mockConstruction(AdjustmentDownloadService.class, (adjustmentDownloadService, context) -> {
                Mockito.when(adjustmentDownloadService.downloadAdjustmentsPdf(anyString(), anyString(), any())).thenThrow(new IllegalArgumentException("invalid parameters"));
            })) {
            Contact contact = new Contact();
            contact.setId("contact");
            annotationHelperMockedStatic.when(()-> AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new AdjustmentDownloadApi().downloadPdf("account", DateConstants.BY_DATE, "12/12/2022",
                    "15/12/2022", "REJECTED", new MockHttpServletResponse(),new MockHttpServletRequest());
        }catch (Exception e){
            Assertions.assertEquals("invalid parameters", e.getMessage());
        }
    }

    @Test
    void downloadPdf_nullOrEmptyResponse_test() {
        Map<String, Object> responseMap = new HashMap<>();
        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AdjustmentDownloadService> adjustmentDownloadServiceMock = Mockito.mockConstruction(AdjustmentDownloadService.class, (adjustmentDownloadService, context) -> {
                Mockito.when(adjustmentDownloadService.downloadAdjustmentsPdf(anyString(), anyString(), any())).thenReturn(responseMap);
            })) {
            Contact contact = new Contact();
            contact.setId("contact");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new AdjustmentDownloadApi().downloadPdf("account", DateConstants.BY_DATE, "12/12/2022",
                    "15/12/2022", "REJECTED", new MockHttpServletResponse(),new MockHttpServletRequest());

            Assertions.assertEquals(false, responseEntity.getBody().isSuccess());
        }
    }

    @Test
    void downloadPdf_valid_test() {
        ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();

        Map<String, Object>responseMap = new HashMap<>();
        responseMap.put(EventConstants.ADJUSTMENT, "adjustment");
        responseMap.put(DownloadUtil.PDF_STREAM, byteArrayOutputStream);

        try(MockedStatic<AnnotationHelper> annotationHelperMockedStatic = Mockito.mockStatic(AnnotationHelper.class);
            MockedConstruction<AdjustmentDownloadService> adjustmentDownloadServiceMock = Mockito.mockConstruction(AdjustmentDownloadService.class, (adjustmentDownloadService, context) -> {
                Mockito.when(adjustmentDownloadService.downloadAdjustmentsPdf(anyString(), anyString(), any())).thenReturn(responseMap);
            })) {
            Contact contact = new Contact();
            contact.setId("contactID");
            annotationHelperMockedStatic.when(()->AnnotationHelper.extractCurrentUserContactFromRequest(any(HttpServletRequest.class))).thenReturn(contact);
            ResponseEntity<GenericResponse> responseEntity = new AdjustmentDownloadApi().downloadPdf("accountID", DateConstants.BY_DATE, "11/12/2022",
                    "13/12/2022", "PENDING", new MockHttpServletResponse(),new MockHttpServletRequest());

            Assertions.assertEquals(true, responseEntity.getBody().isSuccess());
        }
    }
}
