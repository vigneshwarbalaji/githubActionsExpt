package com.yoco.peers.service;

import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ContactImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@Slf4j
public class PeerService {

    public Map<String, Object> getClockedInUsers( String accountID, Contact loggedInContact ) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInContact), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());
        PeopleRelationJDO userPro = new UserImpl().getUserWithoutContact(accountID,loggedInContact.getId());
        Map<String,Object> responseMap = new HashMap<>();

        var clockedInentries = new ReportImpl().getAllClockedInEntriesForAccount(accountID);
        List<String> contactIds = new ArrayList<>();
        List<ReportsDTO> convertedEntries;

        if(!ObjUtils.isEmptyList(clockedInentries)) {
            convertedEntries = clockedInentries.stream().map(event -> new ReportsDTO(event, "", userPro.getTimeZone())).collect(Collectors.toList());
            convertedEntries.stream().forEach(entry -> contactIds.add(entry.getContactID()));
            Map<String, Object> contactMap;
            List<Contact> contactList = new ContactImpl().getContacts(contactIds);
            if (!ObjUtils.isEmptyList(contactList)) {
                contactMap = contactList.stream().collect(Collectors.toMap(Contact::getId, Function.identity()));
                if (!ObjUtils.isNullOrEmpty(contactMap)) {
                    Map<String, Object> finalContactMap = contactMap;
                    convertedEntries.stream().forEach(event-> {
                        if(finalContactMap.containsKey(event.getContactID())) {
                            Contact obj = (Contact) finalContactMap.get(event.getContactID());
                            event.setContact(obj);
                            event.setEmailID(obj.getEmailID());
                        }
                    });
                    responseMap.put(SchedulingKeys.ENTRIES, convertedEntries);
                }
            }
        }
        return responseMap;
    }
}
