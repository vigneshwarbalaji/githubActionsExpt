package com.yoco.commons.constants;

import com.yoco.commons.enums.AppMode;
import com.yoco.commons.utils.GaeUtils;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;
import org.mockito.Mockito;

class DcmConstantsTest {
    @Test
    void DcmConstantsInit_Live_mode(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)){
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.LIVE);
            DcmConstants.initializeDcmConstants(AppMode.LIVE);
            Assertions.assertEquals("https://contacts.anywhere.co/services/signin/v2.0/Account/authenticate_v2?apikey=SEN42",DcmConstants.getContactSkillsetInfoUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/checkIfUserExists?apikey=SEN42",DcmConstants.getUserExistenceInDCMUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/linkContactToAccount/?apikey=SEN42",DcmConstants.getAddExistingContactToDCMUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Skills/goClockDefaultAccount?apikey=SEN42",DcmConstants.getUpdateDcmDefaultDomainUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/createUser?apikey=SEN42",DcmConstants.getCreateUserUnderDCMAccountUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v3.0/objects/Contact/Group?apikey={$accountID}&type=team&limit={$limit}&productID="+ DcmConstants.YOCO_BRAND_ID,DcmConstants.getFetchTeamsForAccountUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v3.0/objects/Contact/Group?apikey={$accountID}&type=team&productID="+ DcmConstants.YOCO_BRAND_ID,DcmConstants.getFetchTeamsForUserUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v3.0/objects/Group/{$teamID}?apikey={$accountID}&productID="+ DcmConstants.YOCO_BRAND_ID,DcmConstants.getFetchTeamInfoUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v3.0/objects/Group?apikey={$accountID}&productID="+ DcmConstants.YOCO_BRAND_ID,DcmConstants.getCreateTeamUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$contactID}/initiateResetPassword?apikey="+ DcmConstants.API_KEY,DcmConstants.getInitResetPasswordUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$verificationID}/checkIfVerificationValid?apikey="+ DcmConstants.API_KEY,DcmConstants.getValidateVerificationIdUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v3.0/objects/Group/{$teamID}/Contact?apikey={$accountID}&operation={$operation}&productID="+DcmConstants.YOCO_BRAND_ID,DcmConstants.getUpdateTeamContactsUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/User/{$contactID}/remove?apikey=SEN42&productID="+DcmConstants.YOCO_BRAND_ID,DcmConstants.getRemoveUserFromAccountUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/updatePassword?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdatePasswordUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$verificationID}/resetPassword?apikey="+DcmConstants.API_KEY,DcmConstants.getResetPasswordUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Contact/getProfilePicUrl?apikey="+DcmConstants.API_KEY,DcmConstants.getFetchProfileImageUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Contact/profilePicture?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdateProfileImageUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/updateUser?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdateDcmContactUrl());
            Assertions.assertEquals("https://contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdateDcmAccountUrl());
        }
    }

    @Test
    void DcmConstantsInit_Staging_mode(){
        try(MockedStatic<GaeUtils> gaeUtilsMockedStatic = Mockito.mockStatic(GaeUtils.class)) {
            gaeUtilsMockedStatic.when(GaeUtils::getAppMode).thenReturn(AppMode.STAGING);
            DcmConstants.initializeDcmConstants(AppMode.STAGING);
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/signin/v2.0/Account/authenticate_v2?apikey=SEN42", DcmConstants.getContactSkillsetInfoUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/checkIfUserExists?apikey=SEN42", DcmConstants.getUserExistenceInDCMUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/linkContactToAccount/?apikey=SEN42", DcmConstants.getAddExistingContactToDCMUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Skills/goClockDefaultAccount?apikey=SEN42", DcmConstants.getUpdateDcmDefaultDomainUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/createUser?apikey=SEN42", DcmConstants.getCreateUserUnderDCMAccountUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Contact/Group?apikey={$accountID}&type=team&limit={$limit}&productID=" + DcmConstants.YOCO_BRAND_ID, DcmConstants.getFetchTeamsForAccountUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Contact/Group?apikey={$accountID}&type=team&productID=" + DcmConstants.YOCO_BRAND_ID, DcmConstants.getFetchTeamsForUserUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Group/{$teamID}?apikey={$accountID}&productID=" + DcmConstants.YOCO_BRAND_ID, DcmConstants.getFetchTeamInfoUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Group?apikey={$accountID}&productID=" + DcmConstants.YOCO_BRAND_ID, DcmConstants.getCreateTeamUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$contactID}/initiateResetPassword?apikey=" + DcmConstants.API_KEY, DcmConstants.getInitResetPasswordUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$verificationID}/checkIfVerificationValid?apikey=" + DcmConstants.API_KEY, DcmConstants.getValidateVerificationIdUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v3.0/objects/Group/{$teamID}/Contact?apikey={$accountID}&operation={$operation}&productID=" + DcmConstants.YOCO_BRAND_ID, DcmConstants.getUpdateTeamContactsUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/User/{$contactID}/remove?apikey=SEN42&productID=" + DcmConstants.YOCO_BRAND_ID, DcmConstants.getRemoveUserFromAccountUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/updatePassword?apikey=" + DcmConstants.API_KEY, DcmConstants.getUpdatePasswordUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$verificationID}/resetPassword?apikey=" + DcmConstants.API_KEY, DcmConstants.getResetPasswordUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Contact/getProfilePicUrl?apikey="+DcmConstants.API_KEY,DcmConstants.getFetchProfileImageUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Contact/profilePicture?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdateProfileImageUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}/updateUser?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdateDcmContactUrl());
            Assertions.assertEquals("https://staging.contacts.anywhere.co/services/data/v2.0/objects/Account/{$accountID}?apikey="+DcmConstants.API_KEY,DcmConstants.getUpdateDcmAccountUrl());
        }
    }
}
