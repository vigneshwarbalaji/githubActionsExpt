package com.yoco.commonservcies;

import com.yoco.project.helper.ProjectTaskHelper;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.util.HashMap;
import java.util.Map;

@Slf4j
@RestController
@RequestMapping("/project")
public class ProjectTaskController {

    @PostMapping("/projectOperationsTaskHandler")
    @ResponseStatus(HttpStatus.OK)
    public void projectTaskHandler(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {

        log.info(" received in project task handler ");

        var byteIn = new ByteArrayInputStream(payload);
        var in = new ObjectInputStream(byteIn);
        Map<String, Object> payloadMap = (HashMap<String, Object>) in.readObject();

        log.info(" payloadMap " + payloadMap);

        ProjectTaskHelper.getProjectTaskHelper().projectOperationsTaskHandler(payloadMap);
    }

}
