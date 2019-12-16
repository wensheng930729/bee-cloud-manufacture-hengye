package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @author dell
 * @version 1.0.0
 * @ClassName AuthSearchTypeListRQ
 * @Description 功能描述
 * @Date 2019/9/27 18:24
 **/

@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("列表搜索Type列表请求参数")
public class AuthSearchTypeListRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("types")
    List<Integer> types;
}
