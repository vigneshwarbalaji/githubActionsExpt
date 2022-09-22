package com.yoco.contact.helper;

import com.yoco.commons.cloudservices.TaskCreator;
import com.yoco.commons.constants.CommonAppProperties;
import com.yoco.commons.utils.CloudTaskUtil;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.util.Map;

@Slf4j
public class ContactTaskInitiator {

    private ContactTaskInitiator(){}

    private static final String CONTACT_OPERATIONS_QUEUE = "contact-operation";

    private static String getContactTaskCallBackUrl(){
        return CommonAppProperties.getAppUrl() + "/task/contact/contactTaskHandler";
    }

    public static void initiateContactUpdateOperationsTaskQueue(Map<String,Object> contactObject) throws IOException {
        log.info(" contactUpdate task queue " + contactObject);
        TaskCreator.createPostTask(CONTACT_OPERATIONS_QUEUE,getContactTaskCallBackUrl(), CloudTaskUtil.convertObjectToByteArray(contactObject));
    }

}
