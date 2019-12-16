package com.bee.platform.cloud.user.rq;

import io.swagger.annotations.ApiModel;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.List;

/**
 * @description: 用户id
 * @author: junyang.li
 * @create: 2019-11-06 14:09
 **/
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
@ApiModel("用户id")
public class UserIdsRQ implements Serializable {

    private static final long serialVersionUID = 3292291176847235853L;
    /**
     * 用户id
     */
    private List<Integer> userIds;
}
