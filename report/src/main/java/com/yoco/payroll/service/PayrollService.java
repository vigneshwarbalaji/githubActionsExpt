package com.yoco.payroll.service;

import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.adjustment.helper.approve.ApproveAdjustmentHelper;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.utils.AccountUtil;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.payroll.helper.adminupdate.PayrollAdminUpdateHelper;
import com.yoco.payroll.helper.PayrollHelper;
import com.yoco.payroll.helper.PayrollTaskInitiator;
import com.yoco.payroll.modal.AdminUpdatePayloadDTO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class PayrollService {

    public Map<String, Object> getUserAcknowledgedEntries(String accountID, Contact loggedInUserContact, String range, String from, String to, String cursor) throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO userPRO = UserImpl.getUserImplInstance().getUserWithoutContact(accountID, loggedInUserContact.getId());
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        boolean hasPayrollAccess = PermissionManager.checkUserHasAdjustmentEditAccess(userPRO);
        Validator.checkArgument(Boolean.FALSE.equals(hasPayrollAccess), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        String timeZone = userPRO.getTimeZone();
        RangeValidator.validateRange(range, from, to);
        var rangeInfoDTO = DateUtil.getRangeDetails(timeZone, range, from, to, accountID);
        return new PayrollHelper().getUserConfirmedPayrollDetails(accountID, timeZone, rangeInfoDTO, cursor);
    }

    public Map<String, Object> getAdminUpdatedEntries(String accountID, Contact loggedInUserContact, String range, String from, String to) throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO userPRO = ApproveAdjustmentHelper.getInstance().validateAndExtractAdminPro(accountID, loggedInUserContact);
        String timeZone = userPRO.getTimeZone();
        RangeValidator.validateRange(range, from, to);
        var rangeInfoDTO = DateUtil.getRangeDetails(timeZone, range, from, to, accountID);
        return new PayrollHelper().getAdminUpdatedPayrollDetails(accountID, timeZone, rangeInfoDTO);
    }

    public Map<String,Object> approvePayrollEvents(String accountID, String contactID, Contact adminContact, String payload) throws IOException, NoSuchAlgorithmException {
        Map<String, Object> responseMap = new HashMap<>();

        PayrollHelper payrollHelper = PayrollHelper.getInstance();
        payrollHelper.validateAdminPro(accountID,adminContact);
        List<String> payrollIds = payrollHelper.validateAndExtractApprovalEntryIDsFromPayload(payload);
        SettingsJDO account = AccountUtil.validateAndExtractAccount(accountID);

        List<Map<String, Object>> payrollEventsList = payrollHelper.getPayrollEvents(payrollIds);

        if (!ObjUtils.isNullOrEmpty(payrollEventsList)) {
            List<String> approvedPayrollIds = payrollHelper.validateAndApprovePayrollEvents(payrollEventsList,account,contactID);

            if (!ObjUtils.isNullOrEmpty(approvedPayrollIds)) {
                PayrollTaskInitiator.initiateApprovePayrollQueue(adminContact, approvedPayrollIds, account.getDisplayTimeFormat());
                responseMap.put(SchedulingKeys.EVENT_IDS, approvedPayrollIds);
            }

        }
        return responseMap;
    }

    public Map<String, Object> updateUserVerifiedEntries(String accountID, String contactID, String entryID, Contact loggedInUserContact) throws IOException, NoSuchAlgorithmException {
        String loggedInContactID = loggedInUserContact.getId();
        Validator.checkArgument(!loggedInContactID.equals(contactID), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        PeopleRelationJDO userPRO = UserImpl.getUserImplInstance().getUserWithoutContact(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        Validator.checkArgument(!UserPROUtil.isUserProActive(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        return new PayrollHelper().updateEntriesWithUserVerifiedPaymentStatus(accountID, contactID, entryID, userPRO);
    }

    public GenericResponse adminUpdateEntry(String accountID, String contactID, String entryID, String payload, Contact loggedInUserContact) throws IOException, NoSuchAlgorithmException {
        GenericResponse response = new GenericResponse();
        PayrollAdminUpdateHelper payrollAdminUpdateHelper = new PayrollAdminUpdateHelper();
        PeopleRelationJDO requesterPro = AdjustmentHelper.validateAndExtractLoggedInPro(accountID, loggedInUserContact);
        AdminUpdatePayloadDTO payloadDTO = new AdminUpdatePayloadDTO(payload, entryID, contactID, requesterPro);
        PeopleRelationJDO userPro = UserPROUtil.getUserProWithContact(accountID, contactID);
        Map<String, Object> overlapResponseMap = payrollAdminUpdateHelper.checkAndGenerateOverlapResponseForAdjustedTime(requesterPro, userPro, payloadDTO);
        if (!ObjUtils.isNullOrEmpty(overlapResponseMap)) {
            response.setData(overlapResponseMap);
            return response;
        }
        new ReportImpl().deleteEvents(List.of(ReportsUtil.getIDFromEvent(payloadDTO.getEvent())));
        return payrollAdminUpdateHelper.createAdminUpdatedEventAndGenerateResponse(requesterPro, userPro, payloadDTO);
    }
}
