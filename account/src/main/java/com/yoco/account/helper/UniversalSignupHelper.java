package com.yoco.account.helper;

import com.yoco.account.enums.ACCOUNT_ERROR_MESSAGE;
import com.yoco.account.modal.AccountCreationPayloadDTO;
import com.yoco.commons.constants.StoragePublicUrl;
import com.yoco.commons.constants.UniversalSignupConstants;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.user.ProUserInfo;
import com.yoco.commons.modal.user.Skillset;
import com.yoco.commons.services.UrlFetcher;
import com.yoco.commons.utils.HeaderUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.commons.validations.Validator;
import java.io.IOException;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import static com.yoco.commons.constants.Commons.SUCCESS;

public class UniversalSignupHelper {
    private UniversalSignupHelper(){}
    public static final String RESP_ALREADY_CREATED_KEY = "alreadyCreated";
    public static final String RESP_ACCOUNT_KEY = "account";
    public static final String RESP_CONTACT_KEY = "contact";
    public static final String RESP_ID_KEY = "id";
    public static final String RESP_CREATED_DATE_KEY = "createdDate";

    public static Map<String, Object> createAccountAndValidateResponse(Map<String, Object> universalSignupPayload) throws IOException {
        Map<String,Object> apiResponse =  UrlFetcher.sendPostRequest(UniversalSignupConstants.getFreeSignupApiUrl(),universalSignupPayload, HeaderUtil.getContentJsonTypeHeader(),UrlFetcher.getHttpClientInstance());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(apiResponse) || !Boolean.TRUE.equals(apiResponse.get(SUCCESS)), COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
        Validator.checkArgument(Boolean.TRUE.equals(apiResponse.get(RESP_ALREADY_CREATED_KEY)), ACCOUNT_ERROR_MESSAGE.ACCOUNT_EXISTS.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty((Map<String, Object>)apiResponse.get(RESP_ACCOUNT_KEY)),COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty((List<Map<String, Object>>)apiResponse.get(RESP_CONTACT_KEY)),COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
        return apiResponse;
    }

    public static SettingsJDO extractAndSaveSettingsJdoFromResponse(Map<String, Object> apiResponse, AccountCreationPayloadDTO payloadDTO) {
        Map<String,Object> accountApiResponseMap = (Map<String, Object>) apiResponse.get(RESP_ACCOUNT_KEY);
        String accountID = (String)accountApiResponseMap.get(RESP_ID_KEY);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID),COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
        var dateAddedLongTime = (Long)accountApiResponseMap.get(RESP_CREATED_DATE_KEY);
        var account = new SettingsJDO(accountID,payloadDTO.getEmailID(),payloadDTO.getAccountName(),payloadDTO.getTimeZone()
                ,payloadDTO.getSource(), payloadDTO.generateRegistrationInfoDetails(),payloadDTO.getSrcRef());
        account.setDateAddedLongTime(dateAddedLongTime);
        account.setDateAdded(new Date(dateAddedLongTime));
        AccountImpl.getAccountImplInstance().saveAccount(account);
        return account;
    }

    public static PeopleRelationJDO extractAndSaveUserProAndContactFromResponse(Map<String, Object> apiResponse, AccountCreationPayloadDTO payloadDTO, SettingsJDO account) {
        List<Map<String,Object>> contactList = (List<Map<String,Object>>) apiResponse.get(RESP_CONTACT_KEY);
        Map<String,Object> contactPersonMap = contactList.stream().filter(contactMap-> "person".equals(contactMap.get("category"))).findFirst().orElse(null);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(contactPersonMap),COMMON_ERROR_RESPONSE.UNABLE_TO_CREATE.value());
        var proUserInfo = new ProUserInfo(account.getPeopleUniquePin(),(String)contactPersonMap.get(RESP_ID_KEY),payloadDTO.getEmailID(), Skillset.ROLE_ADMIN,"","");
        var userPro = new PeopleRelationJDO(proUserInfo,proUserInfo.getContactID(),payloadDTO.getTimeZone(),TimeZoneUtil.getZoneInFormat(payloadDTO.getTimeZone(),TimeZoneUtil.OFFSET_ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH),true,true);
        userPro.setDateAddedLongTime(account.getDateAddedLongTime());
        userPro.setDateAdded(new Date(account.getDateAddedLongTime()));
        userPro.setDateModified(account.getDateAddedLongTime());
        var userContact = new Contact(proUserInfo.getContactID(),proUserInfo.getEmailID(),payloadDTO.getFirstName(),payloadDTO.getLastName(),ObjUtils.isNullOrEmpty(payloadDTO.getPhotoID()) ? StoragePublicUrl.DUMMY_USER_PIC_URL : payloadDTO.getPhotoID(),userPro.getDateAddedLongTime());
        userContact.setDateAddedLongTime(account.getDateAddedLongTime());
        userPro.setContact(userContact);
        UserImpl.getUserImplInstance().savePro(userPro);
        ContactImpl.getContactImplInstance().saveContact(userContact);
        return userPro;
    }
}
