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
@ApiModel(value = "web端忽略磅单入参")
public class WeightIgnoreMachineRq implements Serializable {

    @ApiModelProperty(value = "0 忽略 1 恢复 ")
    @NotNull(message = "未选择Tab选项")
    private Integer isIgnore;

    @ApiModelProperty(value = "磅单业务id")
    @NotEmpty(message = "磅单业务id不能空")
    private String machineId;

    @ApiModelProperty("磅单类型 1 采购 2 销售")
    @NotNull(message = "未选择磅单类型")
    @Min(value = 1,message = "未知磅单类型选项")
    @Max(value = 2,message = "未知磅单类型选项")
    private Integer type;

}
