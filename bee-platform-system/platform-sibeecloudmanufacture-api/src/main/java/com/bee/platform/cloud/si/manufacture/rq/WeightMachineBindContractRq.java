package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;
import org.springframework.format.annotation.DateTimeFormat;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 地磅数据信息保存入参
 * @Date 2019/9/23 19:23
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "web磅单绑定合同号入参")
public class WeightMachineBindContractRq implements Serializable {

    @ApiModelProperty(value = "合同业务id")
    @NotEmpty(message = "合同业务id不能空")
    private String contractBusinessId;

    @ApiModelProperty("磅单类型 1 采购 2 销售")
    @NotNull(message = "未选择磅单类型")
    @Min(value = 1,message = "未知磅单类型选项")
    @Max(value = 2,message = "未知磅单类型选项")
    private Integer type;

    @ApiModelProperty(value = "磅单业务id")
    @NotEmpty(message = "磅单业务id不能空")
    private String machineId;

    @ApiModelProperty(value = "合同编号")
    @NotEmpty(message = "合同编号id不能空")
    private String contractNum;

}
