package com.bee.platform.common.entity;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;

/**
 * <p>
 * 
 * </p>
 *
 * @author jie.zhang123
 * @since 2019-03-04
 */

@Data
@NoArgsConstructor
@Accessors(chain=true)
public class EnterprisesUsers implements Serializable {

    private static final long serialVersionUID = 1L;

    private Integer id;
    private Integer enterpriseId;
    private Integer userId;
    private String  post;
    private Integer isInvite;
    private Integer isActive;
    private String  nickname;
    private String  nicknamePinyin;
    private String  appIds;


    @Override
    public String toString() {
        return "EnterprisesUsers{" +
        ", id=" + id +
        ", enterpriseId=" + enterpriseId +
        ", userId=" + userId +
        ", post=" + post +
        ", isInvite=" + isInvite +
        ", isActive=" + isActive +
        ", nickname=" + nickname +
        ", nicknamePinyin=" + nicknamePinyin +
        ", appIds=" + appIds +
        "}";
    }
}
