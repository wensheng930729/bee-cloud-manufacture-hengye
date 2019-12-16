package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.si.manufacture.Application;
import com.bee.platform.cloud.si.manufacture.entity.PlcRealData;
import lombok.extern.slf4j.Slf4j;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-10-18 15:56
 **/
@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
public class TestPlc {

    @Autowired
    private MqttRealDataMapper mqttRealDataMapper;

    @Autowired
    private PlcRealDataMapper plcRealDataMapper;

    @Test
    public void test() throws Exception{

        List<PlcRealData> list=plcRealDataMapper.selectList(new EntityWrapper<PlcRealData>()
                .where("filed='P2-PC-2' or filed='P2-PC-1'"));
        List<BigDecimal> pc1=new ArrayList<>();
        List<BigDecimal> pc2=new ArrayList<>();
        //遍历
        for (PlcRealData item:list) {
            if("P2-PC-2".equals(item.getFiled())){
                pc2.add(new BigDecimal(item.getValue()));
            }
            if("P2-PC-1".equals(item.getFiled())){
                pc1.add(new BigDecimal(item.getValue()));
            }
        }

        for (int i = 1,t=pc1.size(); i <t; i++) {
            boolean result1=pc1.get(i-1)==pc1.get(i);
            boolean result2=pc2.get(i-1)==pc2.get(i);
            if(result1 && result2){
                log.error("出现重复数据：P2-PC-1={}，P2-PC-2={}",pc1.get(i),pc2.get(i));
            }
        }
        log.error("程序正常执行完成.......");
    }


}
