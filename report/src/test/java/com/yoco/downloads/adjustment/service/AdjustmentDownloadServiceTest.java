package com.yoco.downloads.adjustment.service;

import com.itextpdf.text.DocumentException;
import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.adjustment.modal.GetAdjustmentPayloadDTO;
import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.downloads.adjustment.helper.AdjustmentDownloadHelper;
import com.yoco.downloads.util.DownloadUtil;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedConstruction;
import org.mockito.Mockito;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

import static org.mockito.ArgumentMatchers.*;

class AdjustmentDownloadServiceTest {
    @Test
    void downloadAdjustmentsPdf_noSkillSet_exception_test() throws IOException, NoSuchAlgorithmException, DocumentException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("id");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setSkillsets("{\"skills\":\"skills\"}");
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
                Mockito.when(adjustmentHelper.checkIfTheUserHasRequiredAdjustmentPermission(anyMap())).thenReturn(false);
            })) {
            Contact contact = new Contact();
            contact.setId("id");
            var getAdjustmentDto = new GetAdjustmentPayloadDTO("", DateConstants.BY_DATE, "10/11/2021", "10/11/2021", "", contact);
            Map<String, Object> responseMap = new AdjustmentDownloadService().downloadAdjustmentsPdf("accountID", "PENDING", getAdjustmentDto);
        }catch (Exception e){
            Assertions.assertEquals("No proper skill set", e.getMessage());
        }
    }

    @Test
    void downloadAdjustmentsPdf_valid_test() throws IOException, NoSuchAlgorithmException, DocumentException {
        PeopleRelationJDO userPro = new PeopleRelationJDO();
        userPro.setContactId("id");
        userPro.setUniquepin("123");
        userPro.setTimeZone("Asia/Kolkata");
        userPro.setSkillsets("{\"skills\":\"skills\"}");
        Map<String, Object> pdfMap = new HashMap<>();
        pdfMap.put(DownloadUtil.PDF_STREAM,"pdfStream");
        pdfMap.put(DownloadUtil.FILE_NAME, "filename");
        try(MockedConstruction<UserImpl> userMock = Mockito.mockConstruction(UserImpl.class, (userImplMock, context) -> {
            Mockito.when(userImplMock.getUserWithoutContact(anyString(),anyString())).thenReturn(userPro);
        });
            MockedConstruction<AdjustmentDownloadHelper>AdjustmentDownloadHelperMock = Mockito.mockConstruction(AdjustmentDownloadHelper.class, (adjustmentDownloadHelper, context)->{
                Mockito.when(adjustmentDownloadHelper.getPdfDocumentForDownload(anyString(), anyString(), anyString(), anyString(), anyString(), any())).thenReturn(pdfMap);
            });
            MockedConstruction<AdjustmentHelper> adjustmentHelperMock = Mockito.mockConstruction(AdjustmentHelper.class, (adjustmentHelper, context)->{
                Mockito.when(adjustmentHelper.checkIfTheUserHasRequiredAdjustmentPermission(anyMap())).thenReturn(true);
            })) {
            Contact contact = new Contact();
            contact.setId("id");
            var getAdjustmentDto = new GetAdjustmentPayloadDTO("", DateConstants.BY_DATE, "10/11/2021", "10/11/2021", "", contact);
            Map<String, Object> responseMap = new AdjustmentDownloadService().downloadAdjustmentsPdf("accountID", "PENDING", getAdjustmentDto);
            Assertions.assertEquals("pdfStream", responseMap.get(DownloadUtil.PDF_STREAM));
        }
    }
}
