package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;

/**
 * @ClassName: OutStorageListDTO
 * @Description: java类作用描述
 * @Author: fei.sun
 * @Date: 2019/10/28 16:08
 * @Version: 1.0
 */
@Data
@Accessors(chain = true)
public class OutStorageListDTO {

    private Integer productId;

    private String productName;

    private String time;

    private BigDecimal productNumber;
}
