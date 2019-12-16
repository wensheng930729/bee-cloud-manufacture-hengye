package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息保存入参
 * @Date 2019/9/23 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "确认修改称重数量入参")
public class ConfirmWeightMachineRq implements Serializable {

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "磅单类型 1 采购 2 销售")
    @NotNull(message = "磅单类型不能为空")
    private Integer weightType;

    @ApiModelProperty(value = "称重类型 1 进厂称重 2 出厂称重")
    @NotNull(message = "称重类型选择不能为空")
    private Integer type;

    @ApiModelProperty(value = "进厂重量")
    private BigDecimal inFactoryWeight;

    @ApiModelProperty(value = "出厂重量")
    private BigDecimal outFactoryWeight;

    @ApiModelProperty(value = "进厂重量是否手动录入 0 自动 1手动 2 手动 （设备异常）")
    private Integer inFactoryWeightByManual;

    @ApiModelProperty(value = "出厂重量是否手动录入 0 自动 1手动 2 手动 （设备异常）")
    private Integer outFactoryWeightByManual;

    @ApiModelProperty(value = "称重时间")
    @DateTimeFormat(pattern = "yyyy-MM-dd HH:mm")
    @JsonFormat(timezone = "GMT+8", pattern = "yyyy-MM-dd HH:mm")
    private Date weighingTime;

}
