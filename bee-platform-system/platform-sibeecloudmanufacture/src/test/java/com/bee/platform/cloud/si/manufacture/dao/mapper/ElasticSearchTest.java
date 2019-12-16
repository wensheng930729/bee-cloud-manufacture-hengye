package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.bee.platform.cloud.si.manufacture.Application;
import com.bee.platform.cloud.si.manufacture.entity.ESZElecMeterData;
import com.bee.platform.cloud.si.manufacture.entity.ElectricityMeterData;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.data.elasticsearch.core.ElasticsearchTemplate;
import org.springframework.test.context.junit4.SpringRunner;

import java.util.List;
import java.util.Map;

/**
 * @ClassName: ElasticSearchTest
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/25 16:37
 * @Version: 1.0
 */
@RunWith(SpringRunner.class)
@SpringBootTest(classes = {Application.class})
public class ElasticSearchTest {

    @Autowired
    private ElasticsearchTemplate elasticsearchTemplate;

    @Test
    public void test(){
        elasticsearchTemplate.deleteIndex(ElectricityMeterData.class);
        elasticsearchTemplate.createIndex(ElectricityMeterData.class);
        elasticsearchTemplate.putMapping(ElectricityMeterData.class);
        System.out.println("==========================");

    }
}
