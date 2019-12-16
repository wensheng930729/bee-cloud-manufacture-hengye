package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.ToString;
import lombok.experimental.Accessors;
import org.hibernate.validator.constraints.Range;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;
import java.util.List;

/**
 * @description:
 * @author: junyang.li
 * @create: 2019-10-11 19:40
 **/
@Data
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("plc数据字段")
public class PlcInfoRQ implements Serializable {
    private static final long serialVersionUID = 8984812609459210561L;

    @ApiModelProperty("plc字段相关的配置")
    @NotEmpty(message ="plc字段相关的配置不能为空" )
    private List<PlcFieldConfigRQ> list;
}
