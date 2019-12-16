package com.bee.platform.cloud.user.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NonNull;

import java.math.BigDecimal;

/**
 * @ClassName: MixBagDTO
 * @Description: 确认混袋实体类
 * @Author: fei.sun
 * @Date: 2019/11/26 15:20
 * @Version: 1.0
 */
@Data
@ApiModel("确认混袋实体类")
public class MixBagDTO {

    @ApiModelProperty("主吨袋编号")
    @NonNull
    String majorTonBagNum ;

    @ApiModelProperty("吨袋编号")
    @NonNull
    String tonBagNum ;

    @ApiModelProperty("混袋数量")
    @NonNull
    BigDecimal mixBagAmount;

}
