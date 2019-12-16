package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: OutStorageDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/21 11:28
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
@ApiModel("出库情况实体")
public class OutStorageDTO {

    private String productName;

    private BigDecimal productNumber;

    private String outStorageTime;

}
