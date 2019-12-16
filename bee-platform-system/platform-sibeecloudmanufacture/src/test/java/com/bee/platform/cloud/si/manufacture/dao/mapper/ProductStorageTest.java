package com.bee.platform.cloud.si.manufacture.dao.mapper;

import com.baomidou.mybatisplus.mapper.EntityWrapper;
import com.bee.platform.cloud.si.manufacture.Application;
import com.bee.platform.cloud.si.manufacture.controller.StorageController;
import com.bee.platform.cloud.si.manufacture.dto.*;
import com.bee.platform.cloud.si.manufacture.entity.FinishedProductPendingStorage;
import com.bee.platform.cloud.si.manufacture.entity.StorageInventory;
import com.bee.platform.cloud.si.manufacture.enums.EnumStorage;
import com.bee.platform.cloud.si.manufacture.rq.BuyCarSampleSureRQ;
import com.bee.platform.cloud.si.manufacture.rq.SampleSaveProductSpecRQ;
import com.bee.platform.cloud.si.manufacture.service.LookBoardStorageService;
import com.bee.platform.cloud.si.manufacture.service.MixBagService;
import com.bee.platform.cloud.si.manufacture.service.SampleService;
import com.bee.platform.cloud.si.manufacture.service.StorageService;
import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuySampleService;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import lombok.experimental.Accessors;
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
 * @ClassName: ProductStorageTest
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/9/26 9:45
 * @Version: 1.0
 */
@Slf4j
@RunWith(SpringRunner.class)
@SpringBootTest(classes={Application.class})
public class ProductStorageTest {
    @Autowired
    private StorageService storageService;
    @Autowired
    private BuySampleService buySampleService;
    @Autowired
    private FinishedProductPendingStorageMapper finishedProductPendingStorageMapper;
    @Autowired
    private LookBoardStorageService lookBoardStorageService;
    @Autowired
    private StorageInventoryMapper storageInventoryMapper;
    @Autowired
    private StorageController storageController;
    @Autowired
    private MixBagService mixBagService;

    @Autowired
    private SampleService sampleService;

    @Test
    public void test(){
        String storageTime = "2019-09-26";
        List<FinishedProductPendingStorage> finishedProductPendingStorages =
                finishedProductPendingStorageMapper.selectList(new EntityWrapper<FinishedProductPendingStorage>()
                        .eq("put_storage", EnumStorage.PutStorage.storage.getKey()).eq("furnace_number","一号炉")
                        .eq("furnace_times","一号炉").eq("scheduling","一班")
                        .eq("storage_id",1).addFilter("DATE_FORMAT(storage_time,'%Y-%m-%d') = {0}",storageTime));
        System.out.println("size : "+finishedProductPendingStorages.size()+"  data : "+finishedProductPendingStorages);
    }

    @Test
    public void test1(){
        Integer type = 1;
        Integer goodsType = 4;
        String startTime = "2019-11-21";
        String endTime = "2019-11-28";
        AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
        userInfo.setName("111").setId(1).setFactoryId(1).setOrgId(1500);
        List<LookBoarOutStorageDTO> lookBoarOutStorageDTOS = lookBoardStorageService.selectOutStorage(type, goodsType, startTime, endTime, userInfo);
        System.out.println(lookBoarOutStorageDTOS);
    }

    @Test
    public void test2(){
        AuthPlatformUserInfo userInfo = new AuthPlatformUserInfo();
        userInfo.setName("111").setId(1).setFactoryId(1).setOrgId(1500);
        BuyCarSampleSureRQ carSampleSureRQ = new BuyCarSampleSureRQ();
        carSampleSureRQ.setMachineId("191204027401007").setAssayResult(1).setCarDeductWeight(new BigDecimal("0.2"))
                .setProductSpecId(1797).setProductSpecName("111");
        buySampleService.sureCarSampleResult(userInfo, carSampleSureRQ);
    }
}
