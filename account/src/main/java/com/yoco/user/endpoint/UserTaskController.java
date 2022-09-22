package com.yoco.user.endpoint;

import com.yoco.commons.annotation.declaration.ValidateAccessToken;
import com.yoco.commons.utils.CloudTaskUtil;
import com.yoco.user.helper.skill.UserSkillTaskHelper;
import com.yoco.user.helper.staff.UserStaffTaskHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.io.IOException;
import java.util.Map;

import static com.yoco.commons.utils.CloudTaskUtil.convertByteArrayToObject;

@Slf4j
@ValidateAccessToken
@RestController
@RequestMapping("/task/user")
public class UserTaskController {

    @PostMapping("/proUpdateTaskHandler")
    @ResponseStatus(HttpStatus.OK)
    public void proUpdateTaskHandler(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
        log.info(" payloadMap " + payloadMap);
        UserSkillTaskHelper.getInstance().updateProTaskHelper(payloadMap);
    }

    @PostMapping("/staffTaskHandler")
    @ResponseStatus(HttpStatus.OK)
    public void staffTaskHandler(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        Map<String, Object> payloadMap = (Map<String, Object>) CloudTaskUtil.convertByteArrayToObject(payload);
        log.info(" payloadMap " + payloadMap);
        UserStaffTaskHelper.getInstance().staffOperationsTaskHelper(payloadMap);
    }

    @PostMapping("/delete")
    @ResponseStatus(HttpStatus.OK)
    public void handleUserDeletion(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        UserStaffTaskHelper.getInstance().handleUserDeletion((Map<String,Object>) convertByteArrayToObject(payload));
    }
}
