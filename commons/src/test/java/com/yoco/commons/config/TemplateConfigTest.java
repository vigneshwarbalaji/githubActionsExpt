package com.yoco.commons.config;

import freemarker.template.Configuration;
import freemarker.template.TemplateException;
import freemarker.template.Version;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

class TemplateConfigTest {

    @Test
    void config_test(){
        Configuration config = new TemplateConfig().getConfig();
        Assertions.assertEquals(StandardCharsets.UTF_8.toString(),config.getDefaultEncoding());
        Assertions.assertEquals(new Version(Configuration.VERSION_2_3_0.toString()),config.getIncompatibleImprovements());
    }

    @Test
    void processAndExtractTemplate() throws TemplateException, IOException {
        Map<String,Object> modal = new HashMap<>();
        modal.put("testValue","world");
        String resp = TemplateConfig.processAndExtractTemplate("testTemplate.ftl",modal);
        Assertions.assertEquals("<!Doctype html><html><body><p>hello: world</p></body></html>",resp);
    }
}