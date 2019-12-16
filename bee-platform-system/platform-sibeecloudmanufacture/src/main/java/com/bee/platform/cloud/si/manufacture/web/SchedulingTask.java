package com.bee.platform.cloud.si.manufacture.web;

import com.bee.platform.cloud.si.manufacture.service.manufacturebuy.BuyContractBasicService;
import com.bee.platform.cloud.si.manufacture.service.manufacturesale.SaleContractBasicService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

/**
 * @ClassName SchedulingTask
 * @Description 定时任务
 * @author qhwang
 * @version 1.0.0
 * @Date 2019/10/17 16:26
 */
@Component
public class SchedulingTask {

    @Autowired
    private BuyContractBasicService buyContractBasicService;
    @Autowired
    private SaleContractBasicService saleContractBasicService;

    /**
     * 每隔一个小时刷新合同的在途量和到货量
     */
    @Scheduled(cron = "0 0 0/1 * * ? ")
    public void refreshContractVolume() {
        System.out.println("*****************定时任务START");
        buyContractBasicService.updateContractVolume();
        saleContractBasicService.updateContractVolume();
        System.out.println("*****************定时任务END");
    }

}
