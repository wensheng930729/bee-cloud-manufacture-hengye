package com.bee.platform.common.dto;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * <p>
 * 管理员表
 * </p>
 *
 * @author qhwang123
 * @since 2019-03-14
 */
@Getter
@Setter
@ToString
@Accessors(chain = true)
@NoArgsConstructor
public class PlatformManagersDTO  implements Serializable{


    private static final long serialVersionUID = -2667369688335444742L;
    private Integer id;
    private String username;
    private String phone;
    private String email;
    private String password;
    private String nickname;
    private Date createAt;
    private Date updateAt;

}
