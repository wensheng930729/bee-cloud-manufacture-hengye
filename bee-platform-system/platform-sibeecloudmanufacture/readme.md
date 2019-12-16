项目说明：
platform-sibeecloudmanufacture => 蜜云制造-硅系

#############################################
项目运行有两种方式：
   第一种用命行的方式运行项目的式：--spring.profiles.active=dev
   第二种添加jvm参数运行项目的方式：-Dspring.profiles.active=dev
   
   
###############Swagger2的使用实例###############
1、@Api：用在请求的类上，说明该类的作用
     tags="说明该类的作用"
     value="该参数没什么意义，所以不需要配置"
示例：

@Api(tags="APP用户注册Controller")


2、@ApiOperation：用在请求的方法上，说明方法的作用
@ApiOperation："用在请求的方法上，说明方法的作用"
    value="说明方法的作用"
    notes="方法的备注说明"
示例：

@ApiOperation(value="用户注册",notes="手机号、密码都是必输项，年龄随边填，但必须是数字")

3、@ApiImplicitParams：用在请求的方法上，包含一组参数说明
     @ApiImplicitParams：用在请求的方法上，包含一组参数说明
     @ApiImplicitParam：用在 @ApiImplicitParams 注解中，指定一个请求参数的配置信息       
        name：参数名
        value：参数的汉字说明、解释
        required：参数是否必须传
        paramType：参数放在哪个地方
            · header --> 请求参数的获取：@RequestHeader
            · query --> 请求参数的获取：@RequestParam
            · path（用于restful接口）--> 请求参数的获取：@PathVariable
            · body（不常用）
            · form（不常用）    
        dataType：参数类型，默认String，其它值dataType="Integer"       
        defaultValue：参数的默认值
示例：
@ApiImplicitParams({
    @ApiImplicitParam(name="mobile",value="手机号",required=true,paramType="form"),
    @ApiImplicitParam(name="password",value="密码",required=true,paramType="form"),
    @ApiImplicitParam(name="age",value="年龄",required=true,paramType="form",dataType="Integer")
})
 
5、@ApiModel：用于响应类上，表示一个返回响应数据的信息
     @ApiModel：用于响应类上，表示一个返回响应数据的信息
            （这种一般用在post创建的时候，使用@RequestBody这样的场景，
            请求参数无法使用@ApiImplicitParam注解进行描述的时候）
     @ApiModelProperty：用在属性上，描述响应类的属性


示例:

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
 
import java.io.Serializable;
 
@ApiModel(description= "返回响应数据")
public class RestMessage implements Serializable{
 
    @ApiModelProperty(value = "是否成功")
    private boolean success=true;
    @ApiModelProperty(value = "返回对象")
    private Object data;
    @ApiModelProperty(value = "错误编号")
    private Integer errCode;
    @ApiModelProperty(value = "错误信息")
    private String message;
 
    /* getter/setter */
}

#################################################
/**
 * @Api：用在请求的类上，表示对类的说明
    tags="说明该类的作用，可以在UI界面上看到的注解"
    value="该参数没什么意义，在UI界面上也看到，所以不需要配置"
 * @author zhigang.zhou
 *
 */
@Api(value="/beat", tags="测试接口模块")
@RestController
@RequestMapping("/test")
public class TestSwaggerController {

    /**
     * @ApiOperation：用在请求的方法上，说明方法的用途、作用
    value="说明方法的用途、作用"
    notes="方法的备注说明"
@ApiImplicitParams：用在请求的方法上，表示一组参数说明
    @ApiImplicitParam：用在@ApiImplicitParams注解中，指定一个请求参数的各个方面
        name：参数名
        value：参数的汉字说明、解释
        required：参数是否必须传
        paramType：参数放在哪个地方
            · header --> 请求参数的获取：@RequestHeader
            · query --> 请求参数的获取：@RequestParam
            · path（用于restful接口）--> 请求参数的获取：@PathVariable
            · body（不常用）
            · form（不常用）    
        dataType：参数类型，默认String，其它值dataType="Integer"       
        defaultValue：参数的默认值
  
示列：        
 @ApiImplicitParams({
    @ApiImplicitParam(name="mobile",value="手机号",required=true,paramType="form"),
    @ApiImplicitParam(name="password",value="密码",required=true,paramType="form"),
    @ApiImplicitParam(name="age",value="年龄",required=true,paramType="form",dataType="Integer")
})
 

@ApiResponses：用在请求的方法上，表示一组响应
    @ApiResponse：用在@ApiResponses中，一般用于表达一个错误的响应信息
        code：数字，例如400
        message：信息，例如"请求参数没填好"
        response：抛出异常的类

@ApiModel：用于响应类上，表示一个返回响应数据的信息
            （这种一般用在post创建的时候，使用@RequestBody这样的场景，
            请求参数无法使用@ApiImplicitParam注解进行描述的时候）
    @ApiModelProperty：用在属性上，描述响应类的属性
     */
    @ApiOperation(value="展示首页信息value", notes = "展示首页信息notes")
    @GetMapping("/show")
    public Object showInfo(){
        return "hello world";
    }

    @ApiOperation(value="添加用户信息", notes = "添加用户信息")
    @ApiImplicitParam(name="user", value="User", required = true, dataType = "User")
    @PostMapping("/addUser")
    public Object addUser(@RequestBody User user){
        return "success";
    }

}
#################RestTemplate的经常使用的方式######################

##发送Get请求
@Test
public void getUsers() {
   List<UserEntity> response = restTemplate.exchange("http://localhost:8181/getUsers",
         HttpMethod.GET,
         null,
         new ParameterizedTypeReference<List<UserEntity>>() {}).getBody();
   for(UserEntity user : response){
      System.out.println("昵称:"+user.getNickName());
   }

}
SERVER:
@RequestMapping("/getUsers")
public List<UserEntity> getUsers() {
   List<UserEntity> users=userMapper.getAll();
   return users;
}
对于返回List<T>的请求采用上述的方式是OK的。上面的请求是无参请求。接下来有参请求：

##以表单提交的方式发送POST请求
Client:

@Test
public void pageUsers(){
   HttpHeaders headers = new HttpHeaders();
   //  请勿轻易改变此提交方式，大部分的情况下，提交方式都是表单提交
   headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
   //  封装参数，千万不要替换为Map与HashMap，否则参数无法传递
   MultiValueMap<String, String> params= new LinkedMultiValueMap<String, String>();
   params.add("userName", "zs");
   params.add("userSex", "WOMAN");
   params.add("currentPage", "1");
   params.add("pageSize", "3");
   HttpEntity<MultiValueMap<String, String>> requestEntity = new HttpEntity<MultiValueMap<String, String>>(params, headers);
   Page<UserEntity> response = restTemplate.exchange("http://localhost:8181/getList",
         HttpMethod.POST, requestEntity,
         new ParameterizedTypeReference<Page<UserEntity>>() {}).getBody();
   
List<UserEntity> users = response.getContent();
System.out.println(users.size());
}
这里我遇到的问题就是请求的方式是表单请求还是Payload请求。因为这会直接影响到你服务端的参数接收代码:

 headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
这种情况是表单提交。如果你用表单提交那么服务端参数接受就要用:

@RequestMapping(value = "/getList",method = RequestMethod.POST)
public Page<UserEntity> getList(@ModelAttribute UserParam userParam) {
    List<UserEntity> users=userMapper.getList(userParam);
    long count=userMapper.getCount(userParam);
    Page page = new Page(userParam,count,users);
    return page;
}
服务按代码接受入参不能用@RequestBody否则会报错误信息:

JSON parse error: Cannot deserialize instance of `java.lang.String` out of START_ARRAY token; nested exception is com.fasterxml.jackson.databind.exc.MismatchedInputException: Cannot deserialize instance of `java.lang.String` out of START_ARRAY token大概意思是你的请求入参不能封装为json字符串。

第二种请求方式是payload请求:  注意以下的代码没有经过本人的测试是抄上面仁兄的：

//  请求地址
String url = "http://localhost/mirana-ee/app/login";
RestTemplate client = new RestTemplate();
//  一定要设置header
HttpHeaders headers = new HttpHeaders();
headers.setContentType(MediaType.APPLICATION_JSON_UTF8);
//  将提交的数据转换为String
//  最好通过bean注入的方式获取ObjectMapper
ObjectMapper mapper = new ObjectMapper();
Map<String, String> params= Maps.newHashMap();
params.put("username", "国米");
params.put("password", "123456");
String value = mapper.writeValueAsString(params);
HttpEntity<String> requestEntity = new HttpEntity<String>(value, headers);
//  执行HTTP请求
ResponseEntity<String> response = client.postForEntity(url, requestEntity , String.class );
System.out.println(response.getBody());

重点在这两行:

headers.setContentType(MediaType.APPLICATION_JSON_UTF8);指定请求的类型为JSON

String value = mapper.writeValueAsString(params);将参数转化为字符串

@RequestMapping(value="/login", consumes="application/json", method=RequestMethod.POST)
public Account getAccount(@RequestBody Account account) {
    account.setVersion(new Date());
    return account;
}
##########RestTemplate在本项目中的代码案例###########
package com.bee.platform.common.utils;

import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Component;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.util.ObjectUtils;
import org.springframework.web.client.RestTemplate;

import com.bee.platform.common.config.property.CommonAuthConfigProperties;
import com.bee.platform.common.dto.AuthResourceDetailDTO;
import com.bee.platform.common.dto.UserAuthValidateDTO;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.google.common.collect.Maps;

import lombok.extern.slf4j.Slf4j;

/**
 * @author liang.li
 * @ClassName UserInfoUtils
 * @Description 获取当前登录用户信息
 * @Date 2018-12-27 14:29
 */
@Slf4j
@Component("authUserInfoUtils")
public class UserInfoUtils {
    
    private static final String SIMPLE_USERINFO_PATH = "authPlatformUser/simpleUserInfo";
    private static final String USER_ROLEINFO_PATH = "authPlatformUser/userRoleInfo";
    private static final String VALIDATE_PRIVILEGE_PATH = "authPlatformUser/validate/privilege";
    private static final String MENU_PATH = "authResource/resourcesByUser";
    private static final String SYSTEMCONFIG_PATH = "authPlatformConf/getSystemConf";

    @Autowired
    private RestTemplate restTemplate;
    
    @Autowired
    CommonAuthConfigProperties props;
    
	public String getSystemConf(String key, String value, String desc) {
		// 请求头信息
		HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
		// 封装参数，千万不要替换为Map与HashMap，否则参数无法传递
		MultiValueMap<String, String> params = new LinkedMultiValueMap<String, String>();
		params.add("confKey", key);
		params.add("confValue", value);
		params.add("confDesc", desc);
		HttpEntity<MultiValueMap<String, String>> requestEntity = new HttpEntity<MultiValueMap<String, String>>(params,
				headers);
		// 发送POST请求
		ResponseEntity<ResponseResult<String>> resp = restTemplate.exchange(
				props.getExpectedAuthAddress() + SYSTEMCONFIG_PATH, HttpMethod.POST, requestEntity,
				new ParameterizedTypeReference<ResponseResult<String>>() {
				});
		// 处理响应结果
		if (null != resp && HttpStatus.OK == HttpStatus.valueOf(resp.getStatusCodeValue())) {
			ResponseResult<String> responseResult = resp.getBody();
			if (null != responseResult && ResCodeEnum.SUCCESS.code == responseResult.getCode()) {
				return responseResult.getObject();
			}
		}
		return null;
	}

    /******************公共权限新接口*******************/
    
    /**
     * 获取用户基础信息,不包括角色
     * @param request
     * @return
     */
    public AuthPlatformUserInfo getSimpleUserInfo(HttpServletRequest request) {
        // 获取当前用户信息
        String financeToken = AuthWebUtils.getParam(AuthConstantsUtil.SYS_TOKEN,request);
        if(StringUtils.isBlank(financeToken)){
            log.error("未获取到sys_token");
            return null;
        }
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<AuthPlatformUserInfo>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + SIMPLE_USERINFO_PATH + "?sysToken={sysToken}", HttpMethod.GET, requestEntity, new ParameterizedTypeReference<ResponseResult<AuthPlatformUserInfo>>(){},financeToken);
        AuthPlatformUserInfo userInfo = resp.getBody().getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return null;
        }
        return userInfo;
    }
    
    /**
     * 获取用户信息包括权限信息
     * @param userName 用户名即手机号码
     * @return
     */
    public AuthPlatformUserInfo getUserRoleInfo(String userName) {
        // 获取当前用户信息
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<AuthPlatformUserInfo>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + USER_ROLEINFO_PATH + "?username={username}", HttpMethod.GET, requestEntity, new ParameterizedTypeReference<ResponseResult<AuthPlatformUserInfo>>(){}, userName);
        AuthPlatformUserInfo userInfo = resp.getBody().getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return null;
        }
        return userInfo;
    }
    
    /**
     * 获取用户信息包括权限信息
     * @param sysToken 用户平台token
     * @return
     */
    public AuthPlatformUserInfo getUserRoleInfoByToken(String sysToken) {
        // 获取当前用户信息
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<AuthPlatformUserInfo>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + USER_ROLEINFO_PATH + "?sysToken={sysToken}", HttpMethod.GET , requestEntity, new ParameterizedTypeReference<ResponseResult<AuthPlatformUserInfo>>(){}, sysToken);
        AuthPlatformUserInfo userInfo = resp.getBody().getObject();
        if (ObjectUtils.isEmpty(userInfo)) {
            return null;
        }
        return userInfo;
    }

    
    
    /**
     * 权限验证
     * @param rq
     * @return
     */
    public boolean validatePrivilege(UserAuthValidateDTO rq) {
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_JSON);
     
        HttpEntity<UserAuthValidateDTO> requestEntity = new HttpEntity<>(rq, headers);
        ResponseEntity<ResponseResult<Boolean>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + VALIDATE_PRIVILEGE_PATH, HttpMethod.POST, requestEntity, new ParameterizedTypeReference<ResponseResult<Boolean>>(){});
        if (ResCodeEnum.SUCCESS.getCode().equals(resp.getBody().getCode())) {
            return resp.getBody().getObject().booleanValue();
        } else {
            log.error("权限校验出错, {}", resp.getBody().getMessage());
            return false;
        }
    }
    
    /**
     * 根据用户名userName获取用户菜单
     * @param sysToken
     * @return
     */
    public List<AuthResourceDetailDTO> getMenus(String sysToken) {
        HttpHeaders headers = setInnerCallHeader(MediaType.APPLICATION_FORM_URLENCODED);
        
        Map<String, Object> paramMap=Maps.newHashMap();  
        paramMap.put("subSys", props.getPlatform());
        paramMap.put("sysToken", sysToken);
        HttpEntity<Object> requestEntity = new HttpEntity<>(headers);
        ResponseEntity<ResponseResult<List<AuthResourceDetailDTO>>> resp = restTemplate.exchange(props.getExpectedAuthAddress() + MENU_PATH + "?subSys={subSys}&sysToken={sysToken}", HttpMethod.GET, requestEntity, new ParameterizedTypeReference<ResponseResult<List<AuthResourceDetailDTO>>>(){}, paramMap);
        if (ResCodeEnum.SUCCESS.getCode().equals(resp.getBody().getCode())) {
            return resp.getBody().getObject();
        } else {
            log.error("获取用户菜单出错, {}", resp.getBody().getMessage());
            return null;
        }
    }
    
    /**
     * 设置内部调用头信息
     * @param mt
     * @return
     */
    private HttpHeaders setInnerCallHeader(MediaType mt) {
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(mt);
        headers.add(AuthConstantsUtil.INNER_CLIENT_ID, AuthConstantsUtil.PLATFORM);
        return headers;
    }
    
}
###############################################
项目中相关计算位数精确说明

示例 accountTotal.divide(accountSum, scale, roundingMode);

scale为小数位数；

roundingMode为小数模式,模式如下:

ROUND_CEILING
如果 BigDecimal 是正的，则做 ROUND_UP 操作；如果为负，则做 ROUND_DOWN 操作。
ROUND_DOWN
从不在舍弃(即截断)的小数之前增加数字。
ROUND_FLOOR
如果 BigDecimal 为正，则作 ROUND_UP ；如果为负，则作 ROUND_DOWN 。
ROUND_HALF_DOWN
若舍弃部分> .5，则作 ROUND_UP；否则，作 ROUND_DOWN 。
ROUND_HALF_EVEN
如果舍弃部分左边的数字为奇数，则作 ROUND_HALF_UP ；如果它为偶数，则作 ROUND_HALF_DOWN 。
ROUND_HALF_UP
若舍弃部分>=.5，则作 ROUND_UP ；否则，作 ROUND_DOWN 。
ROUND_UNNECESSARY
该“伪舍入模式”实际是指明所要求的操作必须是精确的，，因此不需要舍入操作。
ROUND_UP
总是在非 0 舍弃小数(即截断)之前增加数字。


#############MyBatis-Plus的CRUD的实例##################################
@Slf4j
@RunWith(SpringRunner.class)   
@SpringBootTest(classes={Application.class})// 指定启动类
public class MybatisPlusCrudDemoTest {
	@Autowired
	private EmployeeMapper employeeMapper;
	
	@Autowired
	private ConfigService configService;

    @Test
    public void tetsGetConfigByconfigKey() {
    	Config config = configService.getConfigByconfigKey("aaKey");
    	log.info("config={}", config);
    }

	// 通用的插入操作
    @Test
    public void testInsert() {
    	Employee entity = new Employee().setLastName("zhigang")
    			.setEmail("xxx@163.com")
    			//.setGender("1")
    			.setAge(20);
		//通用的
    	Integer insertCounts = employeeMapper.insert(entity );
    	log.info("insertCounts={},该条数据的id为={}",insertCounts,entity.getId());
    	/**
    	     INSERT 
		    INTO
		        tbl_employee
		        ( last_name,  email,    age )  
		    VALUES
		        ( 'zhigang',  'xxx@163.com',    20 )
    	 */
    }
    
	// 通用的全字段的插入操作
    @Test
    public void testAllCloumInsert() {
    	Employee entity = new Employee().setLastName("zhigang")
    			.setEmail("xxx@163.com")
    			//.setGender("1")
    			.setAge(20);
		//通用的
    	Integer insertCounts = employeeMapper.insertAllColumn(entity);
    	log.info("insertCounts={},该条数据的id为={}",insertCounts,entity.getId());
    	/**
    	     INSERT 
		    INTO
		        tbl_employee
		        ( last_name,email,gender,age )  
		    VALUES
		        ( 'zhigang','xxx@163.com',null,20 )
    	 */
    }
    
    // 通用的通过id更新操作
    @Test
    public void testUpdateById() {
    	Employee entity = new Employee().setId(1)
    			.setLastName("zhigang-update-1")
    			.setEmail("xxx@163.com")
    			//.setGender("1")
    			.setAge(21);
		//通用的
    	Integer updateCounts = employeeMapper.updateById(entity);
    	log.info("insertCounts={},该条数据的id为={}", updateCounts, entity.getId());
    	/**
    	     UPDATE
		        tbl_employee  
		    SET
		        last_name='zhigang-update-1',
		        email='xxx@163.com',
		        age=21  
		    WHERE
		        id=1
    	 */
    }
    
    // 通用的通过id全字段的更新操作
    @Test
    public void testUpdateAllCloumById() {
    	Employee entity = new Employee().setId(1)
    			.setLastName("zhigang-update-1")
    			.setEmail("xxx@163.com")
    			//.setGender("1")
    			.setAge(21);
		//通用的
    	Integer updateCounts = employeeMapper.updateAllColumnById(entity);
    	log.info("insertCounts={},该条数据的id为={}", updateCounts, entity.getId());
    	/**
    	     UPDATE
		        tbl_employee  
		    SET
		        last_name='zhigang-update-1',
		        email='xxx@163.com',
		        gender=null,
		        age=21  
		    WHERE
		        id=1
    	 */
    }
    
    // 通用的通过指定的某些字段的更新操作
    @Test
    public void testUpdateByCustomerCloum1() {
    	Employee entity = new Employee()
    			.setLastName("zhigang-update-1")
    			.setEmail("xxx@163.com")
    			//.setGender("1")
    			.setAge(28);
		//通用的
    	Integer updateCounts = employeeMapper.update(entity, new EntityWrapper<Employee>().where("email='xxx@163.com' and age=21"));
    	log.info("insertCounts={},该条数据的id为={}", updateCounts, entity.getId());
    	/**
    	     UPDATE
		        tbl_employee  
		    SET
		        last_name='zhigang-update-1',
		        email='xxx@163.com',
		        age=28    
		    WHERE
		        (
		            email='xxx@163.com' 
		            and age=21
		        )
    	 */
    }
    
    // 通用的通过指定的某些字段的更新操作
    @Test
    public void testUpdateByCustomerCloum2() {
    	Employee entity = new Employee()
    			.setLastName("zhigang")
    			.setEmail("xxx@163.com")
    			.setGender("1")
    			.setAge(20);
		//通用的
    	Integer updateCounts = employeeMapper.update(entity, new EntityWrapper<Employee>(new Employee()
    			.setLastName("zhigang-update-y")
    			.setAge(20)));
    	log.info("updateCounts={},该条数据的id为={}", updateCounts, entity.getId());
    	/**
    	     UPDATE
		        tbl_employee  
		    SET
		        last_name='zhigang',
		        email='xxx@163.com',
		        gender='1',
		        age=20    
		    WHERE
		        last_name='zhigang-update-y'       
		        AND age=20
    	 */
    }
    
 // 通用的通过指定的某些字段的更新操作
    @Test
    public void testUpdateAllCloumByCustomerCloum2() {
    	Employee entity = new Employee()
    			.setLastName("zhigang")
    			.setEmail("xxx@163.com")
    			.setGender("1")
    			.setAge(20);
		//通用的
    	Integer updateCounts = employeeMapper.update(entity, new EntityWrapper<Employee>(new Employee()
    			.setLastName("zhigang")
    			.setAge(20)));
    	log.info("updateCounts={},该条数据的id为={}", updateCounts, entity.getId());
    	/**
    	     UPDATE
		        tbl_employee  
		    SET
		        last_name='zhigang',
		        email='xxx@163.com',
		        gender='1',
		        age=20    
		    WHERE
		        last_name='zhigang'       
		        AND age=20
    	 */
    }
	
	// 查询所有
    @Test
    public void testSelectList() {
    	// 查询所有
    	List<Employee> list = employeeMapper.selectList(null);
    	log.info("list={}",list);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee
    	 */
    }
    
    // 根据指定字段查询
    @Test
    public void testSelectListByCustomeFields() {
    	List<Employee> list = employeeMapper.selectList(new EntityWrapper<>(new Employee()
    			.setAge(20)
    			.setEmail("xxx@163.com")));
    	log.info("list={}",list);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee   
		    WHERE
		        email='xxx@163.com'     
		        AND age=20
    	 */
    }
    
    //查询指定id的集合
    @Test
    public void testSelectListByBachIds() {
		List<Employee> list = employeeMapper.selectBatchIds(Arrays.asList("1","2","3")
				.stream()
				.map(Integer::parseInt)
				.collect(Collectors.toList()) );
    	log.info("list={}",list);
        /**
             SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age 
		    FROM
		        tbl_employee 
		    WHERE
		        id IN (
		            1  , 2  , 3  
		        )
         */
    }
    
    
    //根据数据表中的列查询
    @Test
    public void testSelectByMap() {
    	Map<String, Object> columnMap = new HashMap<>();
    	columnMap.put("last_name", "zhigang");
    	columnMap.put("age", 20);
		List<Employee> list = employeeMapper.selectByMap(columnMap );
		log.info("list={}",list);
		/**
		     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age 
		    FROM
		        tbl_employee     
		    WHERE
		        last_name = 'zhigang'   
		        AND age = 20
		 */
    }
    
    // 测试mybatis-plus中的分页查询
    @Test
    public void testSelectPage() {
    	// 当前页码（从1开始）
    	int currentPage = 2;
    	// 每页显示的记录数
    	int size = 3;
		Page<Employee> page = new Page<>(currentPage , size);
    	List<Employee> list = employeeMapper.selectPage(page, null);
    	log.info("list={},page={}", list, page);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee LIMIT 3,
		        3
    	 */
    }
    
    // 测试mybatis-plus中的分页查询
    @Test
    public void testSelectConditionPage() {
    	// 当前页码（从1开始）
    	int currentPage = 1;
    	// 每页显示的记录数
    	int size = 10;
		Page<Employee> page = new Page<>(currentPage , size);
		
    	List<Employee> list = employeeMapper.selectPage(page, new EntityWrapper<>(new Employee()
    			.setAge(20)
    			.setEmail("xxx@163.com")).orNew().or("age=30"));
    	log.info("list={}",list);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee   
		    WHERE
		        email='xxx@163.com'     
		        AND age=20   
		        OR (
		            age=30
		        ) LIMIT 0,10
    	 */
    }
    
    // 测试mybatis-plus中的分页查询
    @Test
    public void testSelectConditionPage1() {
    	List<Employee> list = employeeMapper.selectPage(new Page<>(1 , 10), new EntityWrapper<Employee>()
    			.between("age", 15, 20)
    			.eq("gender", "1")
    			.or("last_name", "zhigang-update-1"));
    	log.info("list={}",list);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee   
		    WHERE
		        (
		            age BETWEEN 15 AND 20 
		            AND gender = '1' 
		            OR last_name
		        ) LIMIT 0,10
    	 */
    }
    
    // 测试mybatis-plus中的EntityWrapper
    @Test
    public void testSelectListWrapper() {
    	List<Employee> list = employeeMapper.selectList(new EntityWrapper<Employee>()
    			.eq("gender", "1")
    			.like("last_name", "zhigang")
    			//前面和后面是or的关系
    			.or()
    			// 与前面是or的关系
    			.like("email", "163"));
    	log.info("list={}",list);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee   
		    WHERE
		        (
		            gender = '1' 
		            AND last_name LIKE '%zhigang%' 
		            OR email LIKE '%163%'
		        )
    	 */
    }
    
    // 测试mybatis-plus中的EntityWrapper
    @Test
    public void testSelectListWrapper2() {
    	List<Employee> list = employeeMapper.selectList(new EntityWrapper<Employee>()
    			.eq("gender", "1")
    			.like("last_name", "zhigang")
    			//前面和后面是or的关系
    			.orNew()
    			// 与前面是or的关系
    			.like("email", "163"));
    	log.info("list={}",list);
    	/**
    	         SELECT
			        id,
			        last_name AS lastName,
			        email,
			        gender,
			        age  
			    FROM
			        tbl_employee   
			    WHERE
			        (
			            gender = '1' 
			            AND last_name LIKE '%zhigang%'
			        )  
			        OR (
			            email LIKE '%163%'
			        )
    	 */
    }
    
    // 测试条件更新
    @Test
    public void testUpdateByCondition() {
    	Employee entity = new Employee()
    			.setLastName("zhigang")
    			.setEmail("xxx@163.com")
    			.setGender("1")
    			.setAge(20);
		//通用的
    	Integer updateCounts = employeeMapper.update(entity, new EntityWrapper<Employee>()
    			.eq("gender", "0")
    			.eq("last_name", "zhigang"));
    	log.info("updateCounts={},该条数据的id为={}", updateCounts, entity.getId());
    	/**
    	     UPDATE
		        tbl_employee  
		    SET
		        last_name='zhigang',
		        email='xxx@163.com',
		        gender='1',
		        age=20    
		    WHERE
		        (
		            gender = '0' 
		            AND last_name = 'zhigang'
		        )
    	 */
    }
    
    // 分组查询
    @Test
    public void testSelectListWrapper3() {
    	List<Employee> list = employeeMapper.selectList(new EntityWrapper<Employee>()
    			.eq("gender", "1")
    			.like("last_name", "zhigang")
    			.orderDesc(Arrays.asList("age")));
    	log.info("list={}",list);
    	/**
    	     SELECT
		        id,
		        last_name AS lastName,
		        email,
		        gender,
		        age  
		    FROM
		        tbl_employee   
		    WHERE
		        (
		            gender = '1' 
		            AND last_name LIKE '%zhigang%'
		        ) 
		    ORDER BY
		        age DESC
    	 */
    }
    
 // 分组查询
    @Test
    public void testSelectListWrapper4() {
    	List<Employee> list = employeeMapper.selectList(new EntityWrapper<Employee>()
    			.eq("gender", "1")
    			.like("last_name", "zhigang")
    			.orderBy("age")
    			 // 可以在sql后面拼接关键字
    			.last("DESC"));
    	log.info("list={}",list);
    	/**
    	         SELECT
			        id,
			        last_name AS lastName,
			        email,
			        gender,
			        age  
			    FROM
			        tbl_employee   
			    WHERE
			        (
			            gender = '1' 
			            AND last_name LIKE '%zhigang%'
			        ) 
			    ORDER BY
			        age DESC
    	 */
    }
    
    // 分组查询
    @Test
    public void testSelectListWrapper5() {
    	List<Employee> list = employeeMapper.selectList(new EntityWrapper<Employee>()
    			.eq("gender", "1")
    			.like("last_name", "zhigang")
    			.orderBy("age")
    			 // 可以在sql后面拼接关键字
    			.last("DESC limit 0,3"));
    	log.info("list={}",list);
    	/**
			    SELECT
			        id,
			        last_name AS lastName,
			        email,
			        gender,
			        age  
			    FROM
			        tbl_employee   
			    WHERE
			        (
			            gender = '1' 
			            AND last_name LIKE '%zhigang%'
			        ) 
			    ORDER BY
			        age DESC limit 0,
			        3
    	 */
    }
    
    // 测试Condition类
    @Test
    public void testSelectConditionClass() {
    	@SuppressWarnings("unchecked")
		List<Employee> list = employeeMapper.selectPage(new Page<Employee>(1 , 10), Condition.create()
    			.between("age", 15, 20)
    			.eq("gender", "1")
    			.or("last_name", "zhigang-update-1"));
      	log.info("list={}",list);
    }
    /**
         SELECT
	        id,
	        last_name AS lastName,
	        email,
	        gender,
	        age  
	    FROM
	        tbl_employee   
	    WHERE
	        (
	            age BETWEEN 15 AND 20 
	            AND gender = '1' 
	            OR last_name
	        ) LIMIT 0,10
     */

    @Test
    public void createModle(){
		TestMP mp=new TestMP();
		mp.testGenerator();
	}
    
}