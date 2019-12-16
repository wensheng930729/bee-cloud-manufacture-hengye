package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 产品档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("修改产品化验项请求参数")
public class ConfigProductTestItemUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("产品id不能为空")
    @NotNull(message = "产品id不能为空")
    private Integer id;

    @ApiModelProperty("产品化验输入项列表")
    @NotNull(message = "产品化验输入项列表至少一条数据,不能为空")
    @Size(min = 1,message = "产品化验输入项至少一条数据")
    private List<ConfigProductTestAttributeInSaveRQ> productTestAttributeInSaveRQS;

    @ApiModelProperty("产品化验结果项列表")
    @NotNull(message = "产品化验结果项列表至少一条数据,不能为空")
    @Size(min = 1,message = "产品化验结果项至少一条数据")
    private List<ConfigProductTestAttributeOutSaveRQ> productTestAttributeOutSaveRQS;




}
