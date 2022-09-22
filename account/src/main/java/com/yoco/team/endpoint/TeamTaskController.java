package com.yoco.team.endpoint;

import com.yoco.commons.modal.dcm.TeamDTO;
import com.yoco.team.helper.TeamTaskHandler;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.*;
import java.io.IOException;
import java.util.Map;
import static com.yoco.commons.utils.CloudTaskUtil.convertByteArrayToObject;

@RestController
@RequestMapping("/task/team")
public class TeamTaskController {
    @PostMapping("/delete")
    @ResponseStatus(HttpStatus.OK)
    public void handleTeamDeletion(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        TeamTaskHandler.handleTeamDeletion((TeamDTO) convertByteArrayToObject(payload));
    }

    @PostMapping("/create")
    @ResponseStatus(HttpStatus.OK)
    public void handleTeamCreation(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        TeamTaskHandler.handleTeamCreation((Map<String,Object>) convertByteArrayToObject(payload));
    }

    @PostMapping("/update/info")
    @ResponseStatus(HttpStatus.OK)
    public void handleTeamInfoUpdate(@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        TeamTaskHandler.handleTeamInfoUpdate((TeamDTO) convertByteArrayToObject(payload));
    }

    @PostMapping("/contact/{operation}")
    @ResponseStatus(HttpStatus.OK)
    public void handleTeamContactsUpdate(@PathVariable String operation,@RequestBody byte[] payload) throws IOException, ClassNotFoundException {
        TeamTaskHandler.handleTeamContactsUpdate((Map<String,Object>) convertByteArrayToObject(payload),operation);
    }
}
