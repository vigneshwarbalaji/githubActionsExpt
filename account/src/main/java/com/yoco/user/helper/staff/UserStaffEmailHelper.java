package com.yoco.user.helper.staff;

import com.yoco.commons.config.TemplateConfig;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.constants.ContactConstants;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.enums.EmailKeys;
import com.yoco.commons.modal.contact.ContactDTO;
import com.yoco.commons.modal.mail.MailAddressDTO;
import com.yoco.commons.modal.mail.MailDTO;
import com.yoco.commons.services.EmailService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.EmailUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.contact.helper.password.PasswordDCMHelper;
import com.yoco.user.helper.UserDownloadHelper;
import freemarker.template.TemplateException;
import lombok.extern.slf4j.Slf4j;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.Map;

@Slf4j
public class UserStaffEmailHelper {

    public static final String COMMA_SEPARATOR = ",";
    public static UserStaffEmailHelper getInstance(){
        return new UserStaffEmailHelper();
    }

    private static final String WELCOME_USER_MAIL_FTL = "welcomeUserMail.ftl";
    private static final String STAFF_IMPORT_ADMIN_MAIL_FTL = "staffImportAdminMail.ftl";


    private String getRedirectUrlForNewUser(String verificationID,String contactID){
        return CommonAppProperties.getYoCoDashboardUrl() + "createnewpassword?code=" + verificationID + "&Id=" + contactID + "&type=new";
    }

    private String getRedirectUrlForExistingUser(String accountID,String contactID){
        return CommonAppProperties.getYoCoDashboardUrl() + "auth/company?accountID=" + accountID + "&contactID=" + contactID;
    }

    private MailAddressDTO getMailAddressesForAddUserMail(String toEmailID){
        return new MailAddressDTO(EmailUtil.MAIL_SENDER, EmailUtil.MAIL_SENDER_NAME, toEmailID, "","","");
    }

    private Map<String,Object> getModalForAddUserMail(String userName,String adminName,String workspaceName,
                                                     String userEmailID,String redirectUrl){
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
        modalMap.put(EmailKeys.USER_NAME.toString(),userName);
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),adminName);
        modalMap.put(EmailKeys.REDIRECT_URL.toString(),redirectUrl);
        modalMap.put(EmailKeys.COMPANY_NAME.toString(),workspaceName);
        modalMap.put(EmailKeys.LOGIN_MAIL.toString(),userEmailID);
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(), DateUtil.getCurrentYear());
        return modalMap;
    }

    private void sendAddUserMail(Map<String,Object> modalMap, MailAddressDTO addressDTO){
        try {
            String subject = EmailUtil.getSubject("You have been invited to be a part of " + modalMap.get(EmailKeys.COMPANY_NAME.toString()));

            String template = TemplateConfig.processAndExtractTemplate(WELCOME_USER_MAIL_FTL,modalMap);

            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject));

        } catch (TemplateException|IOException e) {
            log.info(" exception in sending add user mail helper : " + e.getMessage());
        }
    }

    private void sendWelcomeMailForNewUser(String verificationId,String workspaceName,Contact adminContact,ContactDTO userContact){
        try{
            String adminName = adminContact.getFirstName();
            String userEmailId = userContact.getEmailID();
            String userName = userContact.getFirstName();
            String userContactID = userContact.getId();

            String redirectUrl = this.getRedirectUrlForNewUser(verificationId,userContactID);
            MailAddressDTO addressDTO = this.getMailAddressesForAddUserMail(userEmailId);
            Map<String, Object> modalMap= this.getModalForAddUserMail(userName,adminName,workspaceName,userEmailId,redirectUrl);

            this.sendAddUserMail(modalMap, addressDTO);

        }catch (Exception e){
            log.info(" exception in sending welcome user mail for new user : " + e.getMessage());
        }
    }

    private void sendWelcomeMailForUserToNewWorkSpace(String accountID,String workspaceName,Contact adminContact, ContactDTO userContact){
        try{
            String adminName = adminContact.getFirstName();
            String userEmailId = userContact.getEmailID();
            String userName = userContact.getFirstName();
            String userContactID = userContact.getId();

            String redirectUrl = this.getRedirectUrlForExistingUser(accountID,userContactID);
            MailAddressDTO addressDTO = this.getMailAddressesForAddUserMail(userEmailId);
            Map<String, Object> modalMap= this.getModalForAddUserMail(userName,adminName,workspaceName,userEmailId,redirectUrl);

            this.sendAddUserMail(modalMap, addressDTO);

        }catch (Exception e){
            log.info(" exception in sending welcome user mail for user to new workspace : " + e.getMessage());
        }
    }

    public void addStaffEmailHandler(Map<String,Object> userMap, String accountID, Contact adminContact, ContactDTO userContact) throws NoSuchAlgorithmException, IOException {
        boolean isNewContact = (boolean) userMap.get("isNewContact");
        String workspaceName = (String) userMap.get("accountName");

        if(isNewContact){

            Map<String, Object> pwdResp = PasswordDCMHelper.getVerificationID(userContact.getId());

            if(!ObjUtils.isNullOrEmpty(pwdResp) && pwdResp.containsKey(ContactConstants.VERIFICATION_ID)){
                String verificationID = (String)pwdResp.get(ContactConstants.VERIFICATION_ID);
                this.sendWelcomeMailForNewUser(verificationID,workspaceName,adminContact,userContact);
            }else
                log.info(" invalid response from dcm on initiate reset password ...." + pwdResp);

        }else{
            this.sendWelcomeMailForUserToNewWorkSpace(accountID,workspaceName,adminContact,userContact);
        }
    }


    private void sendImportUserMail(Map<String,Object> modalMap, MailAddressDTO addressDTO, String attachment){
        try {
            String subject = EmailUtil.getSubject("Summary of Invites Sent");

            String template = TemplateConfig.processAndExtractTemplate(STAFF_IMPORT_ADMIN_MAIL_FTL,modalMap);

            log.info("Initiating mail for import users ... ");
            EmailService.getInstance().sendHtmlTemplatedMail(new MailDTO(addressDTO,template,subject,"Failed_User_report.csv",attachment));

        } catch (TemplateException|IOException e) {
            log.info(" exception in sending import user admin mail helper : " + e.getMessage());
        }
    }

    private String getAttachmentDataWithUserImportDetails(Map<String,Object> failedUsersList){
        var failedUsersBuilder = new StringBuilder();
        failedUsersList.forEach( (emailId,reason) -> failedUsersBuilder.append(emailId + COMMA_SEPARATOR)
                .append(reason)
                .append(UserDownloadHelper.CSV_TERMINATOR)
        );
        return failedUsersBuilder.toString();
    }

    private Map<String,Object> getModalForImportUserMail(String adminName,int totalCount,int successCount,String failedCountMsg,
                                                         String supportMail){
        Map<String,Object> modalMap = new HashMap<>();
        modalMap.put(EmailKeys.DOMAIN_IMAGE.toString(), CommonAppProperties.getYoCoDashboardUrl());
        modalMap.put(EmailKeys.ADMIN_NAME.toString(),adminName);
        modalMap.put(EmailKeys.TOTAL_COUNT.toString(),totalCount);
        modalMap.put(EmailKeys.SUCCESS_COUNT.toString(),successCount);
        modalMap.put(EmailKeys.FAILED_COUNT_WITH_MSG.toString(),failedCountMsg);
        modalMap.put(EmailKeys.SUPPORT_MAIL.toString(),supportMail);
        modalMap.put(EmailKeys.CURRENT_YEAR.toString(),DateUtil.getCurrentYear());
        return modalMap;
    }

    public void importStaffEmailHandler(Map<String,Object> failedUsersList, Contact adminContact, int totalCount){
        int failedUsersCount = failedUsersList.size();
        int successCount = totalCount - failedUsersCount;

        String failedCountMsg = failedUsersCount + " ";

        if(failedUsersCount > 0){
            failedCountMsg +=  "&#38; their email addresses are attached below ";
        }

        MailAddressDTO addressDTO = this.getMailAddressesForAddUserMail(adminContact.getEmailID());
        Map<String, Object> modalMap = this.getModalForImportUserMail(adminContact.getFullName(),totalCount,successCount,failedCountMsg,EmailUtil.MAIL_SENDER);

        String attachmentData = this.getAttachmentDataWithUserImportDetails(failedUsersList);
        this.sendImportUserMail(modalMap, addressDTO,attachmentData);
    }

}
