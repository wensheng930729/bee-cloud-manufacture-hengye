package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.math.BigDecimal;
import java.util.List;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 确认磅单信息入参
 * @Date 2019/10/8 9:28
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "确认磅单信息入参")
public class ConfirmMachineRQ implements Serializable {

    @ApiModelProperty(value = "磅单业务id")
    @NotBlank(message = "磅单业务id不能为空")
    private String machineId;

    @ApiModelProperty(value = "磅单类型 1 采购 2 销售")
    @NotNull(message = "磅单类型不能为空")
    private Integer type;

    @ApiModelProperty(value = "扣重数量")
    private BigDecimal deductWeight;

    @ApiModelProperty(value = "备注")
    private String remark;

    @ApiModelProperty(value = "附件信息")
    private List<CommonFileRq> files;
}
