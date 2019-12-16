import { Component } from 'react';
import { Button, Card, Table, Form, Input, Select, Modal, message, Row, Col } from 'antd';
import styles from './index.less';
import withRouter from "umi/withRouter";
import BreadCrumb from '../../../components/BreadCrumb';
import router from 'umi/router';
import {
  getFiles,
  updateFiles,
  addFiles
} from '../services/index';

const FormItem = Form.Item;
const Option = Select.Option;

//仓库档案文件
@withRouter
@Form.create()
export default class Index extends Component {
  state = {
    data: [],
    currentPage: 1,
    pageSize: 10,
    totalPage: 0,
    totalRecords: 0,
    visible: false,
    type: 0, //0新增，1编辑
    editObj: null,
    params:{
      name:'',
    }
  }

  componentDidMount() {
    const { currentPage, pageSize } = this.state;
    const { name } = this.state.params;
    this.getData({ currentPage, pageSize, name: name });
  }

  getData = (params) => {
    console.log(params)
    getFiles(params).then(res => {
      if (res && res.code === 1 && res.object) {
        this.setState({
          data: res.object,
          ...res.page
        })
      } else {
        message.error(res.message);
      }
    })
  }

  onChange = (currentPage) => {
    const { pageSize } = this.state;
    const { name } = this.state.params;
    this.getData({ currentPage, pageSize, name });
  }

  onShowSizeChange = (currentPage, pageSize) => {
    const { name } = this.state.params;
    this.getData({ currentPage, pageSize, name });
  }

  handleAdd = () => {
    const { validateFields, resetFields } = this.props.form;
    const { type, editObj } = this.state;
    let self = this;
    validateFields((err, values) => {
      if (!err) {
        let params = { ...values };

        if (type) {
          params['id'] = editObj.id;
          updateFiles(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
              self.setState({ visible: false, type: 0, editObj: null });
              self.getData({ currentPage: 1, pageSize: 10 });
              resetFields();
            } else {
              message.error(res.message);
            }
          })
        } else {
          addFiles(params).then(res => {
            if (res && res.code === 1) {
              message.success(res.message);
              self.setState({ visible: false, type: 0, editObj: null });
              self.getData({ currentPage: 1, pageSize: 10 });
              resetFields();
            } else {
              message.error(res.message);
            }
          })
        }
      }
    });
  }

  handleSearch = () => {
    const { currentPage, pageSize, params } = this.state;
    this.getData({currentPage, pageSize, name: params.name});
  }

  handleReset = () => {
    const { resetFields } = this.props.form;
    resetFields();
    let { params } = this.state;
    params.name = '';
    this.setState({ params });
    this.getData({ currentPage: 1, pageSize: 10, name: '' });
  }

  onParamsChange = (name,value) => {
    let params = { ...this.state.params }
    params[name] = value
    this.setState({params})
  }

  render() {
    const { getFieldDecorator, resetFields } = this.props.form;
    const { data, currentPage, pageSize, totalPage, totalRecords, visible, type, editObj, params } = this.state;
    const columns = [
      {
        title: '仓库名称',
        dataIndex: 'name',
        key: 'name',
        width: '25%'
      }, {
        title: '类别',
        dataIndex: 'type',
        key: 'type',
        render: (text, row) => text === 0 ? '成品' :
          text === 1 ? '原料' :
            text === 2 ? '配料' :
              text === 3 ? '五金' : '其他',
        width: '25%'
      }, {
        title: '状态',
        dataIndex: 'status',
        key: 'status',
        render: (text, row) => text ? <span style={{ color: '#1890FF' }}>启用</span> : <span style={{ color: '#f5222d' }}>停用</span>,
        width: '25%'
      }, {
        title: ' 操作',
        key: 'actions',
        render: (text, row) => (
          <div>
            <span onClick={() => this.setState({ visible: true, type: 1, editObj: row })} style={{ color: '#1890ff', cursor: 'pointer' }}>编辑</span>
          </div>
        ),
        width: '25%'
      }
    ]

    return (
      <div>
        <BreadCrumb extra={<Button onClick={() => this.setState({ visible: true, type: 0 })} type="primary">新增</Button>} />
        <div className={styles.container}>
          <Card title="仓库档案" bordered={false}>
            {/* <Form layout='inline'> */}
              <Row type="flex" justify="space-between" style={{ width: '100%' }}>
                <Col>
                  {/* <FormItem label="仓库名称"> */}
                    {/* { */}
                      {/* // getFieldDecorator('name')( */}
                        <label>仓库名称</label>&nbsp;&nbsp;<Input style={{ width: 260 }} value={ this.state.params.name } onChange={ e => this.onParamsChange('name',e.target.value) } placeholder="模糊查询"/>
                      {/* // ) */}
                    {/* } */}
                  {/* </FormItem> */}
                </Col>
                <Col>
                  <Button type="primary" onClick={this.handleSearch}>查询</Button>
                  <Button onClick={this.handleReset} style={{ marginLeft: 24 }}>重置</Button>
                </Col>
              </Row>
            {/* </Form> */}
            <Table
              rowKey="id"
              columns={columns}
              dataSource={data}
              pagination={{
                showQuickJumper: true,
                showSizeChanger: true,
                defaultCurrent: 1,
                defaultPageSize: 10,
                current: currentPage,
                pageSize: pageSize,
                total: totalRecords,
                onChange: this.onChange.bind(this),
                pageSizeOptions: ["10", "20", "30"],
                showTotal: (total, range) => `共 ${totalRecords} 条记录 第 ${currentPage} / ${totalPage} 页`,
                onShowSizeChange: this.onShowSizeChange.bind(this)
              }}
            />
          </Card>
        </div>

        <Modal
          title={type ? "编辑仓库" : "新增仓库"}
          visible={visible}
          onOk={this.handleAdd}
          okText="保存"
          onCancel={() => { this.setState({ visible: false, type: 0, editObj: null }) }}
        >
          <Form layout="inline">
            <FormItem label="仓库名称">
              {
                getFieldDecorator('name', {
                  initialValue: type ? editObj.name : null,
                  rules: [{ required: true, message: '请输入仓库名称' }]
                })(
                  <Input style={{ width: 300 }} placeholder="请输入" />
                )
              }
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="仓库类别">
              {
                getFieldDecorator('type', {
                  initialValue: type ? editObj.type : null,
                  rules: [{ required: true, message: '请选择仓库类别' }]
                })(
                  <Select style={{ width: 300 }} placeholder="请选择">
                    <Option value={0}>成品</Option>
                    <Option value={1}>原料</Option>
                    <Option value={2}>配料</Option>
                    <Option value={3}>五金</Option>
                    <Option value={4}>其他</Option>
                  </Select>
                )
              }
            </FormItem>
            <FormItem style={{ marginTop: 24 }} label="启用状态">
              {
                getFieldDecorator('status', {
                  initialValue: type ? editObj.status : 1,
                  rules: [{ required: true, message: '请选择启用状态' }]
                })(
                  <Select style={{ width: 300 }} placeholder="请选择">
                    <Option value={0}>停用</Option>
                    <Option value={1}>启用</Option>
                  </Select>
                )
              }
            </FormItem>
          </Form>
        </Modal>
      </div>
    )
    // 测试一下名字
  }
}