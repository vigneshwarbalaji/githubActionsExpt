package com.yoco.hours.endpoint;

import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.constants.EventConstants;
import com.yoco.hours.helper.EntryDeleteHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/task/entry")
public class HoursTaskController {
    @PostMapping("/delete-handler")
    public void entryDeletionHandler(@RequestBody byte[] payload){
        try{
            Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
            EntryDeleteHelper.handleEntryDeletion((PeopleRelationJDO)payloadMap.get("loggedInUserPro"),(Contact)payloadMap.get("entryContact"),(Map<String,Object>)payloadMap.get(EventConstants.ENTRY));
        }catch (Exception e){
            log.error("Error processing entry deletion queue :: " + e.getMessage());
        }
    }
}
