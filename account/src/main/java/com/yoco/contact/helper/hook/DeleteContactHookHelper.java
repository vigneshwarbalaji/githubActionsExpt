package com.yoco.contact.helper.hook;

import com.fullauth.api.manage.exception.FullAuthApiException;
import com.fullauth.api.manage.iam.AccessPolicy;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.IAMPermission;
import com.yoco.commons.fullservices.iammanagement.AccessManager;
import com.yoco.commons.modal.dcm.DcmContactDTO;
import com.yoco.commons.utils.ActivityUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.user.helper.UserTaskInitiator;
import com.yoco.user.helper.staff.UserStaffHelper;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Slf4j
public class DeleteContactHookHelper {

    public static final String FULL_SYNC_DELETE = "YoCo_Delete_FullSync";

    public static DeleteContactHookHelper getInstance(){
        return new DeleteContactHookHelper();
    }

    public void processContactDeletion(DcmContactDTO dcmContactDTO){
        try{
            log.info(" received contact deletion request, proceeding to validate further ");

            String contactID = dcmContactDTO.getId();

            List<PeopleRelationJDO> userPros = UserImpl.getUserImplInstance().getAllUserProsForUser(contactID,false);
            if(ObjUtils.isNullOrEmpty(userPros)){
                log.info(" there are no active pros for the user, so skipping the request further ");
                return;
            }


            List<AccessPolicy> userPolicies = AccessManager.getAllActiveAccessPoliciesOfUser(contactID);
            if(ObjUtils.isNullOrEmpty(userPolicies)){
                log.info(" there are no active access policies for the user, so skipping the request further ");
                return;
            }

            List<AccessPolicy> superAdminPolicies = userPolicies.stream()
                    .filter(policy -> policy.getPermissions().contains(IAMPermission.SUPER_ADMIN.toString()))
                    .collect(Collectors.toList());

            if(!ObjUtils.isNullOrEmpty(superAdminPolicies)){
                userPros = this.skipSuperAdminProsDeletionProcess(contactID, userPros, superAdminPolicies);
                userPolicies.removeAll(superAdminPolicies);
            }

            if(!ObjUtils.isNullOrEmpty(userPros)){
                UserStaffHelper staffHelper = UserStaffHelper.getInstance();
                userPros.stream().forEach(pro -> processContactDeletionInAccount(staffHelper, pro));
                this.deleteAccessPoliciesOfContact(userPolicies);
            }

        }catch (Exception e){
            log.info(" exception in contact deletion hook helper ... " + e.getMessage());
        }
    }


    public List<PeopleRelationJDO> skipSuperAdminProsDeletionProcess(String contactID, List<PeopleRelationJDO> userPros,
                                                                                List<AccessPolicy> superAdminPolicies) {
        List<String> superAdminsAccountIDs = superAdminPolicies.stream()
                .map(policy -> policy.getResource().split("/")[1])
                .collect(Collectors.toList());

        List<PeopleRelationJDO> primaryAdminPros = this.persistSuperAdminDeletionActivity(contactID, userPros, superAdminsAccountIDs);
        userPros.removeAll(primaryAdminPros);

        return userPros;
    }

    public List<PeopleRelationJDO> persistSuperAdminDeletionActivity(String contactID, List<PeopleRelationJDO> userPros,
                                                                      List<String> superAdminsAccountIDs) {
        return userPros.stream()
                .filter(pro -> superAdminsAccountIDs.contains(pro.getUniquepin()))
                .map(pro -> {
                    ActivityUtil.saveActivity(pro.getUniquepin(), contactID, FULL_SYNC_DELETE, pro.getEmailID(),
                            "MailID "+pro.getEmailID()+" is PrimaryAdmin so not marking existing PRO as deleted",
                            ActivityUtil.ACTIVITIES.DUMMY.value());
                    return pro;
                }).collect(Collectors.toList());
    }

    private void processContactDeletionInAccount(UserStaffHelper staffHelper, PeopleRelationJDO pro) {
        try {
            staffHelper.updateAndSaveProForUserDisabling(pro);
            PeopleRelationJDO adminPro = UserPROUtil.getPrimaryAdmin(pro.getUniquepin());
            UserTaskInitiator.initiateDeleteUserQueue(adminPro, pro, FULL_SYNC_DELETE);
        } catch (Exception e) {
            log.info(" exception in handleContactDeletion " + e.getMessage());
        }
    }

    public void deleteAccessPoliciesOfContact(List<AccessPolicy> policies) throws FullAuthApiException, IOException {
        if(!ObjUtils.isNullOrEmpty(policies)){
            Set<String> policyIdsToDelete = policies.stream()
                    .map(policy -> policy.getResource().split("/")[1])
                    .collect(Collectors.toSet());
            AccessManager.deleteAccessPolicies(policyIdsToDelete);
        }
    }

}
